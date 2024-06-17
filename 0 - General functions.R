# Read configuration file ---------------
read_config <- function(file) {
  lines <- readLines(file)
  lines <- lines[!grepl("^#", lines)]  # Remove lines that are comments
  config <- list()
  for (line in lines) {
    key_value <- strsplit(line, "=")[[1]]
    key <- trimws(key_value[1])
    value <- trimws(key_value[2])
    config[[key]] <- eval(parse(text = value))
  }
  return(config)
}

# Create cov --------------
create_cov <- function(x, ids=NULL) {
  # Extract the relevant loadings and ivol
  if (is.null(ids)) {
    load <- x$fct_load 
    ivol <- x$ivol_vec
  } else {
    load <- x$fct_load[as.character(ids),] 
    ivol <- x$ivol_vec[as.character(ids)]
  }
  # Create the covariance matrix
  (load %*% x$fct_cov %*% t(load) + diag(ivol))
}

# Create lambda ---------------
create_lambda <- function(x, ids) {
  x[ids] |> diag()
}

# Compute expected risk ---------------
expected_risk_fun <- function(ws, dates, cov_list) {
  # Make sure that ids are sorted identically for each method
  ws |> setorder(type, eom, id)
  types <- unique(ws$type)
  w_list <- ws |> split(by = "eom")
  # Compute variance for each porfolios
  dates |> map(.progress = T, function(d) {
    w_sub <- w_list[[as.character(d)]]
    ids <- w_sub$id |> unique()
    sigma <- cov_list[[as.character(d)]] |> create_cov(ids=as.character(ids))
    types |> map(function(x) {
      w <- w_sub[type==x]$w
      pf_var <- t(w) %*% sigma %*% w
      data.table(type=x, pf_var=drop(pf_var))
    }) |> rbindlist() |> mutate(eom=d)
  }) |> rbindlist()
}

# Long horizon returns ------------------
long_horizon_ret <- function(data, h, impute) { # Impute in c("zero", "mean", "median")
  dates <- data[!is.na(ret_exc), .(eom, "merge_date"=eom)] %>% unique()
  ids <- data[!is.na(ret_exc), .(
    start = min(eom),
    end = max(eom)
  ), by = id]
  full_ret <- ids[dates, on = .(start<=merge_date, end>=merge_date), allow.cartesian=T][, c("start", "end") := NULL]
  full_ret <- data[, .(id, eom, ret_exc)][full_ret, on = .(id, eom)]
  full_ret %>% setorder(id, eom)
  cols <- paste0("ret_ld", 1:h)
  for(l in 1:h) {
    full_ret[, paste0("ret_ld", l) := dplyr::lead(ret_exc, l), by = id]
  }
  full_ret[, ret_exc := NULL]
  # Remove rows with all observations missing
  all_missing <- (is.na(full_ret[, cols, with=F]) %>% rowSums() == h)
  print(paste0("All missing excludes ", round(mean(all_missing)*100, 2), "% of the observations"))
  full_ret <- full_ret[all_missing==F] # Is equivalent to assuming that investors new the security would be delisted. Think it's reasonable since we include deliting returns.
  # Impute missing return
  if (impute == "zero") {
    full_ret[, (cols) := lapply(.SD, function(x) if_else(is.na(x), 0, x)), .SDcols=cols]
  }
  if (impute == "mean") {
    full_ret[, (cols) := lapply(.SD, function(x) if_else(is.na(x), mean(x, na.rm=T), x)), .SDcols=cols, by = eom]
  }
  if (impute == "median") {
    full_ret[, (cols) := lapply(.SD, function(x) if_else(is.na(x), median(x, na.rm=T), x)), .SDcols=cols, by = eom]
  }
  return(full_ret)
}

# Portfolio Function ------------------
sigma_gam_adj <- function(sigma_gam, g, cov_type) {
  if (cov_type=="cov_mult") {
    return(sigma_gam*g)
  }
  if (cov_type == "cov_add") {
    return(sigma_gam+diag(diag(sigma_gam)*g))
  }
  if (cov_type == "cor_shrink") {
    stopifnot(abs(g) <= 1)
    sd_vec <- diag(sqrt(diag(sigma_gam)))
    sd_vec_inv <- solve(sd_vec)
    cor_mat <- sd_vec_inv %*% sigma_gam %*% sd_vec_inv
    cor_mat_adj <- cor_mat * (1-g) + diag(nrow(cor_mat))*g
    cov_adj <- sd_vec %*% cor_mat_adj %*% sd_vec
    rownames(cov_adj) <- rownames(sigma_gam)
    colnames(cov_adj) <- colnames(sigma_gam)
    return(cov_adj)
  }
}

initial_weights_new <- function(data, w_type, udf_weights=NULL) {
  if (w_type == "vw") {
    pf_w <- data[, .(id, w_start = me/sum(me)), by = eom]
  }
  if (w_type == "ew") {
    pf_w <- data[, .(id, w_start = 1/.N), by = eom]
  }
  if (w_type == "rand_pos") {
    pf_w <- data[, .(id, eom)][, w_start := runif(.N)][, w_start := w_start / sum(w_start), by = eom]
  }
  if (w_type == "udf") {
    pf_w <- udf_weights[data, on = .(id, eom)]
  }
  pf_w[eom != min(eom), w_start := NA_real_]
  pf_w[, w := NA_real_]
}

# Portfolio function --------------
pf_ts_fun <- function(weights, data, wealth, gam) {
  comb <- data[, .(id, eom, ret_ld1, pred_ld1, lambda)][weights, on = .(id, eom)] 
  comb <- wealth[, .(eom, wealth)][comb, on = "eom"]
  comb[, .(
    inv = sum(abs(w)),
    shorting = sum(abs(w[w<0])),
    turnover = sum(abs(w-w_start)),
    r = sum(w*ret_ld1),
    tc = unique(wealth)/2*sum(lambda*t(w-w_start)^2)
  ), by = eom][, eom_ret := eom+1+months(1)-1][, eom := NULL]
}

# Size-based screen ----------------
# Important: Function modifies in place
size_screen_fun <- function(chars, type) {
  count <- 0 # Count ensures that at least one screen is applied
  # All 
  if (type=="all") {
    print("No size screen")
    chars[valid_data==T, valid_size := T]
    count <- count + 1
  }
  # Top 1000
  if (str_detect(type, "top")) {
    top_n <- type |> str_remove("top") |> as.integer() 
    chars[valid_data==T, me_rank := frank(-me), by = eom]
    chars[, valid_size := (me_rank <= top_n & !is.na(me_rank))]
    chars[, me_rank := NULL]
    count <- count + 1
  }
  # Bottom N
  if (str_detect(type, "bottom")) {
    bot_n <- type |> str_remove("bottom") |> as.integer() 
    chars[valid_data==T, me_rank := frank(me), by = eom]
    chars[, valid_size := (me_rank <= bot_n & !is.na(me_rank))]
    chars[, me_rank := NULL]
    count <- count + 1
  }
  # Size group
  if (str_detect(type, "size_grp_")) {
    size_grp_screen <- type |> str_remove("size_grp_")
    chars[, valid_size := (size_grp==size_grp_screen & valid_data==T)]
    count <- count + 1  
  }
  # Percentile range - min N!
  if (str_detect(type, "perc")) {
    low_p <- str_extract(type, "(?<=low)\\d+") |> as.integer()
    high_p <- str_extract(type, "(?<=high)\\d+") |> as.integer()
    min_n <- str_extract(type, "(?<=min)\\d+") |> as.integer()
    print(paste0("Percentile-based screening: Range ", low_p, "% - ", high_p, "%, min_n: ", min_n, " stocks"))
    # Extract relevant range
    chars[valid_data==T, me_perc := ecdf(me)(me), by = eom]
    chars[, valid_size := (me_perc > low_p/100 & me_perc <= high_p/100 & !is.na(me_perc))] # ecdf never returns 0, which is why lower bound is a strict inequality
    chars[, n_tot := sum(valid_data, na.rm=T), by = eom]
    chars[, n_size := sum(valid_size, na.rm=T), by = eom]
    chars[, n_less := sum(valid_data==T &  me_perc<=low_p/100, na.rm=T), by = eom]
    chars[, n_more := sum(valid_data==T &  me_perc>high_p/100, na.rm=T), by = eom]
    # How many additional stocks from each side?
    chars[, n_miss := pmax(min_n-n_size, 0)]
    chars[, n_below := ceiling(pmin(n_miss/2, n_less))]
    chars[, n_above := ceiling(pmin(n_miss/2, n_more))]
    chars[n_below+n_above < n_miss & n_above>n_below, n_above := n_above + n_miss-n_above-n_below]
    chars[n_below+n_above < n_miss & n_above<n_below, n_below := n_below + n_miss-n_above-n_below]
    chars[, valid_size := (me_perc > (low_p/100-n_below/n_tot) & me_perc <= high_p/100+n_above/n_tot & !is.na(me_perc))]
    if (FALSE) {
      # See that method workds
      chars[, n_size2 := sum(valid_size, na.rm=T), by = eom]
      chars[eom==as.Date("1979-12-31"), .(id, eom, me, me_perc, n_less, n_more, n_tot, n_size, n_size2, n_miss, n_below, n_above, valid_size)][order(me)]
      
      unique(chars[, .(eom, n_size, n_size2, n_tot)]) |> pivot_longer(-eom) |>  ggplot(aes(eom, value, colour=name)) + geom_point() + geom_hline(yintercept = min_n, linetype="dotted")
      chars[valid_size==T, median(me_perc), by = eom] |> ggplot(aes(eom, V1)) + geom_point() + ylim(c(0,1))
    }
    chars[, c("me_perc", "n_tot", "n_size", "n_less", "n_more", "n_miss", "n_below", "n_above") := NULL]
    count <- count + 1
  }
  if (count != 1) {
    stop("Invalid size screen applied!!!!")
  }
}

# Addition/deletion rule ----------------
# Helper
investment_universe <- function(add, delete) {
  n <- length(add)
  included <- logical(n)
  state <- F
  for (i in 2:n) {
    # Include if stock has been valid for 12 months
    if (state==F & add[i]==T & add[i-1]==F) {
      state <- T
    }
    # Exclude if stock has not been valid in past 12 months
    if (state==T & delete[i]==T) {
      state <- F
    }
    included[i] <- state
  }
  return(included)
}
# Function
addition_deletion_fun <- function(chars, addition_n, deletion_n) {
  chars[, valid_temp := (valid_data==T & valid_size==T)]  # Valid without the addition/deletion rule
  chars %>% setorder(id, eom)
  chars[, addition_count := roll::roll_sum(valid_temp, addition_n), by = id]
  chars[, deletion_count := roll::roll_sum(valid_temp, deletion_n), by = id]
  chars[, add := (addition_count==addition_n)]
  chars[is.na(add), add := F]
  chars[, delete := (deletion_count==0)]
  chars[is.na(delete), delete := F]
  chars[, n := .N, by = id]
  chars[n>1, valid := investment_universe(add = add, delete = delete), by = id]
  chars[n==1, valid := F]
  # Ensure that data is valid
  chars[valid_data==F, valid := F]
  # Check Turnover 
  chars[, chg_raw := (valid_temp != lag(valid_temp)), by = id]
  chars[, chg_adj := (valid != lag(valid)), by = id]
  to <- chars[, .(
    raw_n = sum(valid_temp, na.rm=T),
    adj_n = sum(valid, na.rm=T),
    raw = sum(chg_raw, na.rm=T)/sum(valid_temp, na.rm=T),
    adj = sum(chg_adj, na.rm=T)/sum(valid, na.rm=T)
  ), by = eom][!is.nan(raw) & !is.nan(adj) & adj != 0][, .(
    n_months = .N, 
    n_raw = mean(raw_n),
    n_adj = mean(adj_n),
    turnover_raw = mean(raw),
    turnover_adjusted = mean(adj)
  )]
  cat(paste0("Turnover wo addition/deletion rule: ", round(to$turnover_raw*100, 2), "%", 
             "\n", 
             "Turnover w  addition/deletion rule: ", round(to$turnover_adjusted*100, 2), "%", "\n"))
  chars[, c("n", "addition_count", "deletion_count", "add", "delete", "valid_temp", "valid_data", "valid_size", "chg_raw", "chg_adj") := NULL]
} 
