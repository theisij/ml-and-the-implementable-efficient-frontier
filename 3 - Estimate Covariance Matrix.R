# Prepare cluster characteristics ---------------
cluster_data_m <- chars[valid==T][, c("id", "eom", "size_grp", "ff12", features), with=F]
clusters <- unique(cluster_labels$cluster)
cluster_ranks <- clusters %>% lapply(function(cl) {
  chars_sub <- cluster_labels[cluster==cl & characteristic %in% features]
  # print(paste0(cl, ", n: ", nrow(chars_sub)))
  data_sub <- cluster_data_m[, chars_sub$characteristic, with=F]
  for (c in chars_sub$characteristic) {
    dir <- chars_sub[characteristic == c, direction]
    if (dir == -1) {
      data_sub[, (c) := 1-get(c)]
    }
  }
  data.table(x=data_sub %>% rowMeans()) %>% setnames(old = "x", new = cl)
}) %>% bind_cols()
cluster_data_m <- cluster_data_m[, .(id, eom, "eom_ret"=ceiling_date(eom+1, unit = "month")-1, size_grp, ff12)] %>% cbind(cluster_ranks)
# Add Industry/market dummies
if (settings$cov_set$industries) {
  # Create industry dummies
  industries <- sort(unique(cluster_data_m$ff12))
  for (ind in industries) {
    cluster_data_m[, (ind) := as.integer(ff12==ind)]
  }
  ind_factors <- industries
} else {
  # Add market factor
  cluster_data_m[, mkt := 1]
  ind_factors <- "mkt"
}
# Standardize factors
cluster_data_m[, (clusters) := lapply(.SD, function(x) (x-mean(x))/sd(x)), by=eom, .SDcols=clusters]
# Add daily return data
cluster_data_d <- cluster_data_m[daily[date>= min(cluster_data_m$eom)][, .(id, date, ret_exc, "eom_ret"=eom)], on = .(id, eom_ret)]
# Omit observations without data
cluster_data_d <- cluster_data_d %>% na.omit()

# Create factor returns --------------------
fct_ret_est <- cluster_data_d %>%
  group_by(date) %>%
  nest() %>%
  mutate(
    fit = data %>% map(~lm(paste0("ret_exc~-1+", paste0(c(ind_factors, clusters), collapse = "+")), data=.x)),
    # fit = data %>% map(~lm(paste0("ret_exc~-1+mkt+", paste0(clusters, collapse = "+")), data=.x)),
    res = fit %>% map(residuals),
    tidied = fit %>% map(broom::tidy)
  ) 
fct_ret <- fct_ret_est %>%
  unnest(tidied) %>%
  select(date, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  arrange(date) %>%
  setDT()

# Factor Risk -----------------------------
w_cor <- (0.5^(1/settings$cov_set$hl_cor))^(settings$cov_set$obs:1)
w_var <- (0.5^(1/settings$cov_set$hl_var))^(settings$cov_set$obs:1)

fct_dates <- sort(fct_ret$date)
calc_dates <- sort(unique(cluster_data_m[eom>=floor_date(fct_dates[settings$cov_set$obs], unit="m")-1]$eom)) # to ensure sufficient data for cov calculation
factor_cov_est <- as.character(calc_dates) %>% map(.progress = T, function(d){
  first_obs <- min(tail(fct_dates[fct_dates <= as.Date(d)], settings$cov_set$obs))
  cov_data <- fct_ret[date >= first_obs & date <= as.Date(d)]
  t <- nrow(cov_data)
  if (t < settings$cov_set$obs-30) warning("INSUFFICIENT NUMBER OF OBSERVATIONS!!") # Only an issue with the first calc_date
  cov_est <- cov_data %>% select(-date) %>% cov.wt(wt = tail(w_cor, t), cor=T, center=T, method = "unbiased")
  cor_est <- cov_est$cor
  var_est <- cov_data %>% select(-date) %>% cov.wt(wt = tail(w_var, t), cor=F, center=T, method = "unbiased") # inefficient solution but super fast with few factors
  sd_diag <- diag(sqrt(diag(var_est$cov)))
  # Prepare cov
  cov_est <- sd_diag %*% cor_est %*% sd_diag
  rownames(cov_est) <- colnames(cov_est) <- colnames(cor_est)
  # Output
  return(cov_est)
})
names(factor_cov_est) <- as.character(calc_dates)

# Specific Risk ---------------------------
spec_risk <- fct_ret_est %>%
  mutate(id = data %>% map(~.x$id)) %>% 
  select(id, date, res) %>% 
  unnest(c(id, res)) %>%
  arrange(id, date) %>%
  setDT()
# EWMA variance
spec_risk[, res_vol := ewma_c(res, lambda = 0.5^(1/settings$cov_set$hl_stock_var), start = settings$cov_set$initial_var_obs), by = id] # Lambda=exp(log(0.5)/half_life)
# Require that the observation at least 200 non-missing observations out of the last 252 trading days
td_range <- data.table(date=fct_dates, td_252d = lag(fct_dates, 252))
spec_risk <- td_range[spec_risk, on = "date"]
spec_risk[, date_200d := lag(date, 200), by = id]
spec_risk <- spec_risk[date_200d>=td_252d & !is.na(res_vol)][, .(id, date, res_vol)]
# Extract specific risk by month end
spec_risk[, eom_ret := date %>% ceiling_date(unit="month")-1]
spec_risk[, max_date := max(date), by = .(id, eom_ret)]
spec_risk_m <- spec_risk[date==max_date, .(id, "eom"=eom_ret, res_vol)]

# Stock covariance matrix --------------------------
barra_cov <- as.character(calc_dates) %>% map(.progress = T, function(d){
  char_data <- cluster_data_m[eom == as.Date(d)]
  # Add specific risk
  char_data <- spec_risk_m[char_data, on = .(id, eom)]
  # char_data <- char_data[!is.na(res_vol)]  # Consider if we should do some imputation e.g. median res_vol?
  char_data[, med_res_vol := median(res_vol, na.rm=T), by = .(size_grp, eom)]
  if (any(is.na(char_data$med_res_vol))) {
    char_data[, med_res_vol_all := median(res_vol, na.rm=T), by = .(eom)] # Crazy edge case: in April 1996, there's only 1 micro stock and it has na for resvol..
    char_data[is.na(med_res_vol), med_res_vol := med_res_vol_all]  
  }
  char_data[is.na(res_vol), res_vol := med_res_vol]
  fct_cov <- factor_cov_est[[d]]*21 # Annualize
  char_data %>% setorder(id)  # Important to align ids across different datasets
  X <- char_data[, colnames(fct_cov), with=F] %>% as.matrix() # Using colnames(fct_cov) is important so char and factor is aligned across x and fct_cov
  rownames(X) <- char_data$id %>% as.character()
  # Stock covariance matrix
  # cov_est <- X %*% fct_cov %*% t(X) + diag(char_data$res_vol^2)
  # rownames(cov_est) <- colnames(cov_est) <- as.character(char_data$id)
  # return(cov_est)
  ivol_vec <- char_data$res_vol^2*21 # Annualize. Also, would more accurately be called "ivar_vec"
  names(ivol_vec) <- char_data$id %>% as.character()
  list("fct_load"=X, "fct_cov"=fct_cov, "ivol_vec"=ivol_vec)
})
names(barra_cov) <- as.character(calc_dates)

# Sanity check
if (FALSE) {
  # Check implied volatility of market factor over time
  check <- names(barra_cov) |> map(function(d) {
    data.table(
      eom = as.Date(d),
      # mkt_vol = barra_cov[[d]]$fct_cov["Hlth", "Hlth"]^0.5,
      mkt_vol = barra_cov[[d]]$fct_cov["mkt", "mkt"]^0.5,
      ivol = mean(barra_cov[[d]]$ivol_vec^0.5)
    )
  }) |> rbindlist()
  
  check |> 
    pivot_longer(-eom) |> 
    ggplot(aes(eom, value*sqrt(12), color=name)) +
    geom_line()
  
  # Why we impute
  test <- calc_dates %>% sapply(simplify = F, USE.NAMES = T, function(d){
    char_data <- cluster_data_m[eom == d]
    # Add specific risk
    char_data <- spec_risk_m[char_data, on = .(id, eom)]
    n_miss <- char_data[is.na(res_vol), .N]
    data.table(d=d, n_miss=n_miss)
  }) %>% rbindlist()
  test %>% ggplot(aes(d, n_miss)) + geom_point()
  # Sanity check
  pred_sd_avg <- names(barra_cov) %>% lapply(function(d) {
    pred_var <- diag(barra_cov[[d]])
    data.table(eom = as.Date(d), n = length(pred_var), sd_avg = sqrt(mean(pred_var)*252))
  }) %>% rbindlist()
  pred_sd_avg %>% ggplot(aes(eom, n)) + geom_point(size=1) + theme(axis.title.x = element_blank())
  pred_sd_avg %>% ggplot(aes(eom, sd_avg)) + geom_point(size=1) + labs(y = "Average Predicted Volatility (Annualized)", title = "Covariance Sanity Check") + theme(axis.title.x = element_blank())
  
  
  (output$cov_check <- pred_sd_avg %>% 
    ggplot(aes(eom, sd_avg)) + 
    geom_point(size=1) + 
    labs(y = "Average Predicted Volatility (Annualized)") + 
    theme(axis.title.x = element_blank()))
  
  me <- chars[!is.na(me), .(id, eom, me)]
  me %>% setorder(eom)
  me <- me %>% split(by="eom")
  mkt_vol <- names(barra_cov) %>% lapply(function(d) {
    sigma <- barra_cov[[d]]
    me_sub <- me[[d]]
    me_sub <- me_sub[id %in% colnames(sigma)]
    me_sub[, w := me/sum(me)]
    sigma <- sigma[as.character(me_sub$id), as.character(me_sub$id)]
    data.table(eom=as.Date(d), mkt_vol = sqrt(drop(t(me_sub$w) %*% sigma %*% me_sub$w)))
  }) %>% rbindlist()
  
  (output$mkt_vol <- mkt_vol %>% ggplot(aes(eom, mkt_vol*sqrt(252))) + geom_point() + geom_hline(yintercept = 0))
  
  # Check: All valid stocks have covariance estimate
  valid_cov_est <- names(barra_cov) %>% lapply(function(d) {
    data.table(eom=as.Date(d), id = barra_cov[[d]] %>% colnames() %>% as.integer(), valid_cov = T)
  }) %>% rbindlist()
  test <- valid_cov_est[chars[, .(id, eom, valid)], on = .(id, eom)]
  test[valid==T & eom>=min(valid_cov_est$eom), .N, by = valid_cov]
}