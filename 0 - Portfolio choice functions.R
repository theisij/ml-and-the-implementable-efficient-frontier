# Functions used in several methods ------------------------------------------------
m_func <- function(w, mu, rf, sigma_gam, gam, lambda, iter) {
  n <- nrow(lambda)
  g_bar <- rep(1, n)
  mu_bar_vec <- rep(1+rf+mu, n)
  sigma_gr <- (1+rf+mu)^-2 * (mu_bar_vec %*% t(mu_bar_vec) + sigma_gam/gam)
  lamb_neg05 <- diag(diag(lambda)^(-0.5))  # Only works because lambda diagonal, general solution: Re(sqrtm_cpp(solve(lambda)))
  x <- w^-1*lamb_neg05 %*% sigma_gam %*% lamb_neg05 # Placeholder
  y <- diag(1+diag(sigma_gr))
  # Iterate on F
  sigma_hat <- (x + diag(1+g_bar))  # Here and below I don't account for xi because of the assumption that xi=g_bar=[1, ..., 1]
  m_tilde <- 0.5*(sigma_hat-Re(sqrtm_cpp(sigma_hat%^%2-4*diag(n))))
  for (i in 1:iter) { 
    m_tilde <- solve(x + y - m_tilde * sigma_gr)
  }
  # Output 
  lamb_neg05 %*% m_tilde %*% sqrt(lambda)
}

m_static <- function(sigma_gam, w, lambda, phi) {
  solve(sigma_gam+w/phi*lambda) %*% (w/phi*lambda)
}

# Benchmark portfolios -------------------------------------------------------------
# Generic weight function
w_fun <- function(data, dates, w_opt, wealth) {
  dates_prev <- data.table(eom = dates, eom_prev = lag(dates))
  w <- data %>% initial_weights_new(w_type = "vw") %>% select(-w)
  w <- w_opt[w, on = .(id, eom)]
  w <- dates_prev[w, on = "eom"]
  w_opt <- data[, .(id, eom, tr_ld1)][w_opt, on =.(id, eom)]
  w_opt <- wealth[, .(eom, mu_ld1)][w_opt, on =.(eom)]
  w_opt[, w := w*(1+tr_ld1)/(1+mu_ld1)]
  w_opt %>% setnames(old = c("w", "eom"), new = c("w_prev", "eom_prev"))
  w <- w_opt[w, on = .(id, eom_prev)]
  w[eom != min(eom), w_start := w_prev]
  w[is.na(w_start), w_start := 0]
  w[, c("eom_prev", "w_prev") := NULL]
  return(w)
}
# Tangency portfolio ------------
tpf_implement <- function(data, cov_list, wealth, dates, gam) {
  # Get data subset
  data_rel <- data[valid==T & eom %in% dates, .(id, eom, me, tr_ld1, pred_ld1)][order(id, eom)]
  # Desired weights
  data_split <- data_rel %>% split(by="eom")
  tpf_opt <- as.character(dates) %>% lapply(function(d) {
    data_sub <- data_split[[d]]
    ids <- data_sub$id
    sigma <- cov_list[[d]] |> create_cov(ids = ids)
    data_sub[, .(id, eom, w = 1/gam * drop(solve(sigma) %*% pred_ld1))]
  }) %>% rbindlist()
  # Actual weights
  tpf_w <- data_rel %>% w_fun(wealth=wealth, dates = dates, w_opt = tpf_opt)
  # Portfolio
  tpf_pf <- tpf_w %>% pf_ts_fun(data=data, wealth = wealth, gam = gam) %>% mutate(type = "Markowitz-ML")
  # Output
  list("w"=tpf_w, "pf"=tpf_pf)
}

# Counterfactual TPF -----------
tpf_cf_fun <- function(data, cf_cluster, er_models, cluster_labels, wealth, gamma_rel, cov_list, dates, seed) {
  set.seed(seed) # Ensures reproducability and that the same firms are shuffled for easier comparison across clusters
  # Shuffle chars and predict counterfactual ER
  if (cf_cluster != "bm") {
    cf <- data %>% select(-starts_with("pred_ld")) # Counterfactual
    cf[, id_shuffle := sample(id, .N, replace=F), by = eom]
    chars_sub <- cluster_labels[cluster==cf_cluster & characteristic %in% features, characteristic]
    chars_data <- cf %>% select(all_of(c("id", "eom", chars_sub))) %>% rename("id_shuffle"=id)
    cf <- chars_data[cf[, (chars_sub) := NULL], on = .(id_shuffle, eom)]
    # Expected Returns
    for (m_sub in er_models) {
      sub_dates <- unique(m_sub$pred$eom)
      cf_x <- cf[eom %in% sub_dates, features, with=F] %>% as.matrix()
      cf_new_x <- cf_x %>% rff(W=m_sub$W)
      cf_new_x <- m_sub$opt_hps$p^(-0.5)*cbind(cf_new_x$X_cos, cf_new_x$X_sin) 
      cf[eom %in% sub_dates, pred_ld1 := drop(m_sub$fit %>% predict(newx = cf_new_x, s = m_sub$opt_hps$lambda))]
    }
  } else {
    cf <- data[valid==T]
  }
  # Implement on counterfactual data
  op <- cf |> 
    tpf_implement(cov_list = cov_list, wealth = wealth, dates = dates, gam = gamma_rel) 
  # Output
  op$pf |> 
    mutate(cluster = cf_cluster)
}

# Mean-variance efficient portfolios of risky assets ----------
mv_risky_fun <- function(data, cov_list, wealth, dates, gam, u_vec) {
  # Get data subset
  data_rel <- data[valid==T & eom %in% dates, .(id, eom, me, tr_ld1, pred_ld1)][order(id, eom)]
  # Desired weights
  data_split <- data_rel %>% split(by="eom")
  mv_opt_all <- as.character(dates) %>% map(function(d) {
    data_sub <- data_split[[d]]
    ids <- data_sub$id
    sigma_inv <- cov_list[[d]] |> create_cov(ids = ids) |> solve()
    er <- data_sub[, pred_ld1]
    # Auxiliary constants
    ones <- rep(1, length(er))
    a <- drop(t(er) %*% sigma_inv %*% er)
    b <- sum(sigma_inv%*%er)
    c <- sum(sigma_inv %*% ones)
    d <- a*c-b^2
    # Weights
    u_vec %>% map(function(u) {
      w <- data_sub[, .(id, eom, u=u*12, w = drop((c*u-b)/d*sigma_inv%*%er + (a-b*u)/d * sigma_inv %*% ones))]
    }) %>% rbindlist()
  }) %>% rbindlist()
  # Portfolio
  unique(mv_opt_all$u) |> map(function(u_sel) {
    w <- data_rel %>% w_fun(wealth=wealth, dates = dates, w_opt = mv_opt_all[u==u_sel][, u := NULL])
    w |> pf_ts_fun(data = data, wealth = wealth, gam = gam) |> mutate(u = u_sel)
  }) |> rbindlist()
}


# High minus low -------------------------
factor_ml_implement <- function(data, wealth, dates, n_pfs, gam) {
  # Desired HML weight
  data_split <- data[valid==T & eom %in% dates, .(id, eom, me, pred_ld1)] %>% split(by="eom")
  hml_opt <- as.character(dates) %>% lapply(function(d) {
    data_sub <- data_split[[d]]
    er <- data_sub$pred_ld1
    er_prank <- ecdf(er)(er)
    position <- data_sub[, .(id, eom, me, pos = case_when(
      er_prank >= 1-1/n_pfs ~ 1,
      er_prank <= 1/n_pfs ~ -1,
      TRUE ~ 0
    ))]
    position[, w := me/sum(me)*pos, by = pos][, c("me", "pos") := NULL]
  }) %>% rbindlist()
  # Actual weights
  hml_w <- data[eom %in% dates & valid==T] %>% w_fun(wealth = wealth, dates = dates, w_opt = hml_opt)
  # Portfolio
  hml_pf <- hml_w %>% pf_ts_fun(data = data, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(type = "Factor-ML")
  # Output
  list("w"=hml_w, "pf"=hml_pf)
}

# 1/N implementation ------------------
ew_implement <- function(data, wealth, dates) {
  # Optimal weights
  ew_opt <- data[valid==T & eom %in% dates][, n := .N, by = eom][, .(id, eom, w = 1/n)]
  # Weights
  ew_w <- data[eom %in% dates & valid==T] %>% w_fun(wealth = wealth, dates = dates, w_opt = ew_opt)
  # Portfolio
  ew_pf <- ew_w %>% pf_ts_fun(data = data, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(type = "1/N")
  # Output
  list("w"=ew_w, "pf"=ew_pf)
}

# Market implementation ---------------
mkt_implement <- function(data, wealth, dates) {
  # Optimal weights
  mkt_opt <- data[valid==T & eom %in% dates][, sum_me := sum(me), by = eom][, .(id, eom, w = me/sum_me)]
  # Weights
  mkt_w <- data[eom %in% dates & valid==T] %>% w_fun(wealth = wealth, dates = dates, w_opt = mkt_opt)
  # Portfolio
  mkt_pf <- mkt_w %>% pf_ts_fun(data = data, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(type = "Market")
  # Output
  list("w"=mkt_w, "pf"=mkt_pf)
}

# Rank-weighted portfolio -------------
rw_implement <- function(data, wealth, dates) {
  # Desired rank-weighted weight
  data_split <- data[valid==T & eom %in% dates, .(id, eom, me, pred_ld1)] %>% split(by="eom")
  rw_opt <- as.character(dates) %>% lapply(function(d) {
    data_sub <- data_split[[d]]
    er <- data_sub$pred_ld1
    er_rank <- frank(er)
    pos <- (er_rank-mean(er_rank))
    pos <- pos * 2/sum(abs(pos))
    data_sub[, .(id, eom, w = pos)]
  }) %>% rbindlist()
  # Actual weights
  rw_w <- data[eom %in% dates & valid==T] %>% w_fun(wealth = wealth, dates = dates, w_opt = rw_opt)
  # Portfolio
  rw_pf <- rw_w %>% pf_ts_fun(data = data, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(type = "Rank-ML")
  # Output
  list("w"=rw_w, "pf"=rw_pf)
}

# Minimu, variance portfolio -------------
mv_implement <- function(data, cov_list, wealth, dates) {
  # Desired rank-weighted weight
  data_split <- data[valid==T & eom %in% dates, .(id, eom, me)] %>% split(by="eom")
  mv_opt <- as.character(dates) %>% lapply(function(d) {
    data_sub <- data_split[[d]]
    sig <- cov_list[[d]] |> create_cov(ids = as.character(data_sub$id))
    sig_inv <- solve(sig)
    ones <- matrix(data = 1, nrow = nrow(sig_inv))
    pos <- drop(1/(t(ones) %*% sig_inv %*% ones)) * sig_inv %*% ones
    data_sub[, .(id, eom, w = drop(pos))]
  }) %>% rbindlist()
  # Actual weights
  mv_w <- data[eom %in% dates & valid==T] %>% w_fun(wealth = wealth, dates = dates, w_opt = mv_opt)
  # Portfolio
  mv_pf <- mv_w %>% pf_ts_fun(data = data, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(type = "Minimum Variance")
  # Output
  list("w"=mv_w, "pf"=mv_pf)
}


# STATIC-ML FUNCTIONS --------------------------------------------------------------
# Static - validation
static_val_fun <- function(data, dates, cov_list, lambda_list, wealth, cov_type, gamma_rel, k=NULL, g=NULL, u=NULL, hps=NULL) {
  # Weights [VW if first obs, else use existing weights]
  static_weights <- data %>% initial_weights_new(w_type = "vw")
  static_weights <- data[, .(id, eom, tr_ld1, pred_ld1)][static_weights, on = .(id, eom)]
  static_weights <- wealth[, .(eom, mu_ld1)][static_weights, on = "eom"]
  for(d in as.character(dates)) {
    # If given HPs use those
    if (!is.null(hps)) {
      hp <- hps[year(eom_ret)<year(as.Date(d))][eom_ret == max(eom_ret)]
      g <- hp$g; u <- hp$u; k <- hp$k
    }
    # Build inputs 
    wealth_t <- wealth[eom == as.Date(d)]$wealth
    ids <- static_weights[eom==as.Date(d)]$id
    sigma_gam <- (cov_list[[d]] |> create_cov(ids = as.character(ids)))*gamma_rel 
    sigma_gam <- sigma_gam |> sigma_gam_adj(g = g, cov_type=cov_type)
    lambda <- lambda_list[[d]] |> create_lambda(ids = as.character(ids))
    lambda <- lambda*k
    static_weights[eom==as.Date(d), w := solve(sigma_gam+wealth_t*lambda) %*% (pred_ld1*u+wealth_t*lambda %*% w_start)]
    # Update start weights next month
    next_month <- dates[match(as.Date(d), dates)+1]
    static_weights <- static_weights[eom==as.Date(d), .(id, eom=next_month, "w_opt_lag1"=w*(1+tr_ld1)/(1+mu_ld1))][static_weights, on = .(id, eom)]
    static_weights[eom==next_month, w_start := w_opt_lag1]
    static_weights[eom==next_month & is.na(w_start), w_start := 0]
    static_weights[, w_opt_lag1 := NULL]
  }
  return(static_weights)
}

# Static-full implementation
static_implement <- function(data_tc, cov_list, lambda_list, rf,
                             wealth, mu, gamma_rel,
                             dates_full, dates_oos, dates_hp, hp_years,
                             k_vec, u_vec, g_vec, cov_type, 
                             validation=NULL, seed) {
  # HP grid
  static_hps <- expand.grid(k=k_vec, u=u_vec, g=g_vec)
  # Relevant data 
  data_rel <- data_tc[valid==T & eom %in% dates_hp, .(id, eom, me, tr_ld1, pred_ld1)][order(id, eom)]
  # Validation
  if (is.null(validation)) {
    validation <- 1:nrow(static_hps) %>% lapply(function(i) {
      print(i)
      hp <- static_hps[i,]
      static_w <- data_rel %>% 
        static_val_fun(dates = dates_hp, cov_list = cov_list, lambda_list=lambda_list, wealth=wealth, gamma_rel=gamma_rel, k = hp$k, g=hp$g, u=hp$u, cov_type = cov_type)
      static_w %>% pf_ts_fun(data=data_tc, wealth = wealth, gam = gamma_rel) %>% mutate(hp_no=i, k=hp$k, g=hp$g, u=hp$u)
    }) %>% rbindlist()
  }
  # Optimal hps
  validation %>% setorder(hp_no, eom_ret)
  validation[, cum_var := cummean(r^2)-cummean(r)^2, by = hp_no]
  validation[, cum_obj := cummean(r-tc-0.5*cum_var*gamma_rel), by = hp_no]
  validation[, rank := frank(-cum_obj), by = .(eom_ret)]
  print(validation %>% 
          mutate(name = paste0("u:", u, "k:", round(k, 2), "g:", g)) %>%
          filter(year(eom_ret)>=1981) %>%
          group_by(hp_no) %>%
          mutate(min_rank = min(rank)) %>%
          filter(min_rank <=1) %>% 
          ggplot(aes(eom_ret, cum_obj*12, colour = name)) +
          geom_line())
  optimal_hps <- validation[month(eom_ret)==12 & rank==1][order(eom_ret)]
  # Implement final portfolio 
  w <- data_tc[eom %in% dates_oos & valid==T, .(id, eom, me, tr_ld1, pred_ld1)] %>% 
    static_val_fun(dates = dates_oos, cov_list = cov_list, lambda_list=lambda_list, wealth=wealth, gamma_rel=gamma_rel, hps = optimal_hps, cov_type = cov_type)
  pf <- w %>% pf_ts_fun(data=data_tc, wealth = wealth, gam = gamma_rel) %>% mutate(type = "Static-ML*")
  # Output
  list("hps"=validation, "best_hps"=optimal_hps, "w"=w, "pf"=pf)
}

# Portfolio-ML functions -------------------------------------------------------------------------------------
# Inputs ------
pfml_input_fun <- function(data_tc, cov_list, lambda_list, gamma_rel, wealth, mu, dates, lb, scale, 
                           risk_free, features, rff_feat, seed, p_max, g, add_orig, iter, balanced) {
  # Full Lookback dates 
  dates_lb <- seq.Date(from = min(dates)+1-months(lb+1), to = max(dates)+1, by = "month")-1 # For lookback
  # Create random features 
  if (rff_feat) {
    set.seed(seed)
    rff_x <- data_tc[, features, with=F] %>% rff(p = p_max, g = g)
    rff_w <- rff_x$W
    rff_x <- cbind(rff_x$X_cos, rff_x$X_sin)
    colnames(rff_x) <- c(paste0("rff", 1:(p_max/2), "_cos"), paste0("rff", 1:(p_max/2), "_sin")) 
    data <- data_tc[, .(id, eom, valid, ret_ld1, tr_ld0, mu_ld0)] %>% cbind(rff_x)
    feat_new <- colnames(rff_x)
    if (add_orig) {
      data <- data %>% cbind(data_tc[, features, with=F]) # Important to keep all stocks such that we have the lookback data
      feat_new <- c(feat_new, features)
    }
  } else {
    data <- data_tc[, c("id", "eom", "valid", "ret_ld1", "tr_ld0", "mu_ld0", features), with=F]
    feat_new <- features
  }
  feat_cons <- c(feat_new, "constant") # Feature + constant
  # Add scales 
  if (scale) {
    # Build scales for lookback
    scales <- dates_lb %>% lapply(function(d) {
      sigma <- cov_list[[as.character(d)]] |> create_cov()
      diag_vol <- sqrt(diag(sigma))
      data.table(id=as.numeric(rownames(sigma)), eom=d, vol_scale=diag_vol)
    }) |> rbindlist()
    # Add scales to data
    data <- scales[data, on=.(id, eom)]
    # Impute missing sigmas with cross-sectional mean
    data[, vol_scale := if_else(is.na(vol_scale), median(vol_scale, na.rm=T), vol_scale), by = eom]
  }
  
  if (balanced) {
    # Demean and add constant (if working with a balanced panel)
    data[, (feat_new) := lapply(.SD, function(x) x-mean(x)), by=eom, .SDcols = feat_new]
    data[, constant := 1]
    # Scale all columns to have a constant squared sum of 1 such that the sum of a column is unaffected by the number of stocks (otherwise leverage over time would be dependent on the number of stocks)
    data[, (feat_cons) := lapply(.SD, function(x) x*sqrt((1/sum(x^2)))), by = eom, .SDcols = feat_cons] 
  }
  # Signals and realizations 
  inputs <- as.character(dates) %>% map(function(d) {
    if(year(as.Date(d)) %% 10 == 0 & month(as.Date(d))==1) print(paste0("--> PF-ML inputs: ", d))
    # Prepare inputs for data
    data_ret <- data[valid == T & eom == as.Date(d), .(id, ret_ld1)]
    ids <- data_ret$id
    n <- length(ids)
    r <- data_ret$ret_ld1
    sigma <- cov_list[[d]] |> create_cov(ids = as.character(ids))
    lambda <- lambda_list[[d]] |> create_lambda(ids = as.character(ids))
    w <- wealth[eom == as.Date(d), wealth]
    rf <- risk_free[eom == as.Date(d), rf]
    m <- m_func(w = w, mu=mu, rf=rf,
                sigma_gam = sigma*gamma_rel, gam=gamma_rel, lambda=lambda, iter = iter)
    # Signals ---------
    data_sub <- data[id %in% ids & eom >=floor_date(as.Date(d), unit = "month")-months(lb)-1 & eom<=as.Date(d)] 
    if (!balanced) {
      # If unbalanced, need to scaled after excluding stocks with missing data
      # Demean and add constant
      data_sub[, (feat_new) := lapply(.SD, function(x) x-mean(x)), by=eom, .SDcols = feat_new]
      data_sub[, constant := 1]
      # Scale all columns to have a constant squared sum of 1 such that the sum of a column is unaffected by the number of stocks (otherwise leverage over time would be dependent on the number of stocks)
      data_sub[, (feat_cons) := lapply(.SD, function(x) x*sqrt((1/sum(x^2)))), by = eom, .SDcols = feat_cons] 
    }
    data_sub <- data_sub %>%
      arrange(-as.numeric(eom), id) %>% # Ensure that data_sub[1]==d
      split(by="eom")
    
    signals <- names(data_sub) %>% lapply(function(d_new) {
      # d_new <- dt[, unique(eom)]
      s <- data_sub[[d_new]][, feat_cons, with=F] |> as.matrix()
      if (scale) {
        scales <- (1/data_sub[[d_new]]$vol_scale) |> diag()
        s <- scales %*% s
      }
      # Output
      return(s)
    })
    names(signals) <- names(data_sub)
    # Weighting ---------
    gtm <- data_sub %>% lapply(function(dt) {
      gt <- (1+dt$tr_ld0) / (1+dt$mu_ld0)
      gt[is.na(gt)] <- 1 # Set missing to 1 i.e. assume that their return equaled the portfolio return
      m %*% diag(gt)
    })
    gtm_agg <- vector("list", length = length(gtm)-1)
    names(gtm_agg) <- names(gtm)[-length(gtm)]
    gtm_agg[[1]] <- diag(n)
    gtm_agg_l1 <- gtm_agg
    if (lb>0) {
      for (i in 1:lb) {
        gtm_agg[[i+1]] <- gtm_agg[[i]] %*% gtm[[i]]
        gtm_agg_l1[[i+1]] <- gtm_agg_l1[[i]] %*% gtm[[i+1]]
      }
    }
    # Weighted signals (omega) ------------
    omega <- 0
    const <- 0
    omega_l1 <- 0
    const_l1 <- 0
    for (i in 0:lb) {
      d_new <- as.Date(d)+1-months(i)-1 
      d_new_l1 <- d_new+1-months(1)-1
      s <- signals[[as.character(d_new)]]
      s_l1 <- signals[[as.character(d_new_l1)]]
      omega <- omega + gtm_agg[[as.character(d_new)]] %*% s
      const <- const + gtm_agg[[as.character(d_new)]]
      omega_l1 <- omega_l1 + gtm_agg_l1[[as.character(d_new)]] %*% s_l1
      const_l1 <- const_l1 + gtm_agg_l1[[as.character(d_new)]]
    }
    omega <- solve(const) %*% omega
    omega_l1 <- solve(const_l1) %*% omega_l1
    gt <- diag((1+data_sub[[d]]$tr_ld0) / (1+data_sub[[d]]$mu_ld0)) 
    omega_chg <- omega-gt%*%omega_l1
    rownames(omega_chg) <- ids
    rownames(omega) <- ids
    # Realizations -------------
    # Exp1: r_tilde
    r_tilde <- t(omega) %*% r
    # Exp2: Denominator
    risk <- gamma_rel * (t(omega) %*% sigma %*% omega)
    tc <- w * (t(omega_chg) %*% lambda %*% omega_chg)
    denom <-  risk + tc
    # Reals
    reals <- list("r_tilde"=drop(r_tilde), "denom"=denom, "risk"=risk, "tc"=tc)
    # Signal t
    signal_t <- signals[[as.character(d)]]
    # Output
    op <- list(
      "reals"=reals,
      "signal_t"=signal_t
    )
    # if (rff_feat) op$W <- rff_w # Don't think it's used?
    return(op)
  })
  reals <- inputs |> map("reals")
  names(reals) <- as.character(dates)
  signal_t <- inputs |> map("signal_t")
  names(signal_t) <- as.character(dates)
  # Output
  list("reals"=reals, "signal_t"=signal_t, "rff_w"=rff_w)
}

# Feature names
pfml_feat_fun <- function(p, orig_feat) {
  feat <- c("constant")
  if (p!=0) {
    feat <- c(feat, paste0("rff", 1:(p/2), "_cos"), paste0("rff", 1:(p/2), "_sin"))
  }
  if (orig_feat) feat <- c(feat, features)
  return(feat)
}

# Compute mean of denom matrices before hp_years
denom_sum_fun <- function(train) {
  denom_sum <- lapply(train, function(x) x$denom)
  denom_sum <- Reduce("+", denom_sum)
}

# Hyper search for best coefficients ---------------------
pfml_search_coef <- function(pfml_input, p_vec, l_vec, hp_years, orig_feat) {
  # First, I compute the mean of the r_tilde/denom matrices before hp_years
  d_all <- as.Date(names(pfml_input$reals))
  end_bef <- min(hp_years)-1
  train_bef <- pfml_input$reals[d_all < as.Date(paste0(end_bef-1, "-12-31"))] # < and not <= because returns are from eom+months(1)
  r_tilde_sum <- train_bef %>% sapply(simplify = T, function(x) x$r_tilde) %>% rowSums() # First observations doesn't have a realization 
  denom_raw_sum <- train_bef |> denom_sum_fun()
  n <- length(train_bef)
  rm(train_bef)
  # Second, I update the average by hp-year, and compute the resulting coefficients 
  hp_years <- sort(hp_years) # Make sure that hp_years are sorted!!
  coef_list <- vector("list", length(hp_years))
  names(coef_list) <- hp_years %>% as.character()
  for (i in 1:length(hp_years)) {
    end <- hp_years[i]
    train_new <- pfml_input$reals[d_all >= as.Date(paste0(end-2, "-12-31")) & d_all <= as.Date(paste0(end-1, "-11-30"))]
    # Update n
    n <- n + length(train_new)
    # Update r_tilde
    r_tilde_new <- train_new %>% sapply(simplify = T, function(x) x$r_tilde) %>% rowSums() 
    r_tilde_sum <- r_tilde_sum + r_tilde_new
    # Update denom
    denom_raw_new <- train_new |> denom_sum_fun()
    denom_raw_sum <- denom_raw_sum + denom_raw_new
    # Find coefficients
    coef_by_hp <- p_vec |>  map(function(p) {
      feat_p <- pfml_feat_fun(p = p, orig_feat = orig_feat)
      r_tilde_sub <- r_tilde_sum[feat_p]/n  
      denom_sub <- denom_raw_sum[feat_p, feat_p]/n  
      results <- l_vec |> map(function(l) {
        solve(denom_sub + l*diag(p+1)) %*% r_tilde_sub
      })
      names(results) <- l_vec
      return(results)
    })
    names(coef_by_hp) <- p_vec
    coef_list[[i]] <- coef_by_hp
  }
  # Output
  return(coef_list)
}

# Next, compute realized utility for each p in each hp_year -------------------
pfml_hp_reals_fun <- function(pfml_input, hp_coef, p_vec, l_vec, hp_years, orig_feat) {
  validation <- hp_years %>% lapply(function(end) {
    reals_all <- pfml_input$reals[as.Date(names(pfml_input$reals))>= as.Date(paste0(end-1, "-12-31")) & as.Date(names(pfml_input$reals)) <= as.Date(paste0(end, "-11-30"))]
    coef_list_yr <- hp_coef[[as.character(end)]]
    p_vec |> lapply(function(p) {
      feat_p <- pfml_feat_fun(p = p, orig_feat = orig_feat)
      coef_list_p <- coef_list_yr[[as.character(p)]]
      reals <- reals_all |> lapply(function(x) list(r_tilde=x$r_tilde[feat_p], denom=x$denom[feat_p, feat_p]))
      1:length(l_vec) |> map(function(i) {
        coef <- coef_list_p[[i]]
        names(reals) %>% lapply(function(nm) {
          x <- reals[[nm]]
          r <- t(x$r_tilde) %*% coef - 0.5*t(coef) %*% x$denom %*% coef
          data.table(eom=as.Date(nm), eom_ret=as.Date(nm)+1+months(1)-1, obj=drop(r))
        }) %>% rbindlist() |> mutate(l=l_vec[i])
      }) %>% rbindlist() |> mutate(p=p)
    }) %>% rbindlist() |> mutate(hp_end=end)
  }) |> rbindlist()
  # Compute rank
  validation |> setorder(p, l, eom_ret)
  validation[, cum_obj := cummean(obj), by = .(p, l)]
  validation[, rank := frank(-cum_obj), by = .(eom_ret)]
}

# Create optimal aim portfolio (for a given g) -----------------
pfml_aims_fun <- function(pfml_input, validation, data_tc, hp_coef, hp_years, dates_oos, l_vec, orig_feat) {
  opt_hps <- validation[month(eom_ret)==12 & rank==1, .(hp_end = year(eom_ret), l=l, p=p)]
  aim_pfs_list <- dates_oos %>% lapply(function(d) {
    d_ret <- d+1+months(1)-1
    oos_year <- year(d_ret)
    hp_year <- oos_year-1
    hps_d <- opt_hps[hp_end==hp_year]
    
    feat <- pfml_feat_fun(p = hps_d$p, orig_feat = orig_feat)
    s <- pfml_input$signal_t[[as.character(d)]][, feat]
    l_no <- match(hps_d$l, l_vec) # Lambda can be difficult to match because of floating point errors
    coef <- hp_coef[[as.character(oos_year)]][[as.character(hps_d$p)]][[l_no]]
    aim_pf <- data_tc[valid==T & eom==d, .(id, eom, w_aim = drop(s %*% coef))] # Note: Coefficients and signal align because [, feat] in m2_data_sub
    # Output
    list("aim_pf"=aim_pf, "coef"=coef)
  })
  names(aim_pfs_list) <- dates_oos
  # Output
  return(aim_pfs_list)
}

# Portfolio-ML weights -----------------------------------
pfml_w <- function(data, dates, cov_list, lambda_list, gamma_rel, iter=iter, risk_free, wealth, mu, aims) {
  # Create Aim Portfolio
  if (is.null(aims)) {
    aims <- as.character(dates) %>% lapply(function(d) {
      data_d <- data[eom==as.Date(d), .(id, eom)]
      s <- signal_t[[d]]
      if(is.list(aim_coef)) {
        coef <- aim_coef[[as.character(year(as.Date(d)))]] 
      } else {
        coef <- aim_coef
      }
      data_d$w_aim <- drop(s %*% coef)
      return(data_d)
    }) %>% rbindlist()
  }
  # Weights [VW if first obs, else use existing weights]
  fa_weights <- data %>% initial_weights_new(w_type = "vw")
  fa_weights <- data[, .(id, eom, tr_ld1)][fa_weights, on = .(id, eom)]
  fa_weights <- wealth[, .(eom, mu_ld1)][fa_weights, on = "eom"]
  for (d in as.character(dates)) {
    # print(d)
    # Create m
    ids <- data[eom==as.Date(d), id]
    sigma <- cov_list[[d]] |> create_cov(ids = as.character(ids))
    lambda <- lambda_list[[d]] |> create_lambda(ids = as.character(ids))
    w <- wealth[eom == as.Date(d), wealth]
    rf <- risk_free[eom == as.Date(d), rf]
    m <- m_func(w = w, mu=mu, rf=rf,
                sigma_gam = sigma*gamma_rel, gam=gamma_rel, lambda=lambda, iter = iter)
    iden <- diag(nrow(m))
    w_cur <- aims[fa_weights[eom==as.Date(d)], on = .(id, eom)]
    w_cur$w_opt <- m %*% w_cur$w_start + (iden-m) %*% w_cur$w_aim
    
    # Update weight matrix
    next_month <- dates[match(as.Date(d), dates)+1]
    fa_weights <- w_cur[, .(id, w_opt, w_opt_ld1 = w_opt*(1+tr_ld1)/(1+mu_ld1))][fa_weights, on= .(id)]
    fa_weights[!is.na(w_opt) & eom==as.Date(d), w := w_opt] # This months optimal weights
    fa_weights[!is.na(w_opt) & eom==next_month, w_start := w_opt_ld1] # Next months start weights
    fa_weights[eom==next_month & is.na(w_start), w_start := 0] # New stocks are initialized at a zero weight
    fa_weights[, c("w_opt", "w_opt_ld1") := NULL]
  } 
  return(fa_weights)
}


# Portfolio-ML Implementation ----------------------------
pfml_implement <- function(
    data_tc, cov_list, lambda_list, risk_free, features, 
    wealth, mu, gamma_rel, dates_full, dates_oos, lb, 
    hp_years, rff_feat, scale, 
    g_vec=NULL, p_vec=NULL, l_vec=NULL, orig_feat=NULL, iter, hps=NULL, balanced, seed) {
  # Hyperparameter search ----------
  if (is.null(hps)) {
    start_time <- proc.time()
    # Search over g, p, and l
    hps <- g_vec %>% map(function(g) {
      start_g <- proc.time()
      # Step 1: Prepare data --------------------------------
      print(system.time(pfml_input <- data_tc %>% pfml_input_fun(
        wealth = wealth, mu = mu, dates = dates_full,  
        lb = lb, scale = scale, risk_free = risk_free, 
        cov_list = cov_list, lambda_list = lambda_list, gamma_rel = gamma_rel, iter=iter,
        features = features, balanced = balanced,
        rff_feat = rff_feat, p_max = max(p_vec), g = g, add_orig = orig_feat, seed=seed
      )))
      # RFF weights
      rff_w <- pfml_input$rff_w
      # Inputs 
      feat_all <- pfml_feat_fun(p = max(p_vec), orig_feat = orig_feat)
      pfml_input <- list( # <- To ensure that matrices and vectors are aligned similarly
        reals = pfml_input$reals %>% lapply(function(x) c(list(r_tilde=x$r_tilde[feat_all]), x[-1] %>% lapply(function(z) z[feat_all, feat_all]))),
        signal_t = pfml_input$signal_t %>% lapply(function(x) x[, feat_all])
      )
      # Coefficients 
      pfml_hp_coef <- pfml_input |> pfml_search_coef(p_vec = p_vec, l_vec = l_vec, hp_years = hp_years, orig_feat = orig_feat)
      # Realized utility
      validation <- pfml_input |> pfml_hp_reals_fun(hp_coef=pfml_hp_coef, p_vec=p_vec, l_vec=l_vec, hp_years=hp_years, orig_feat=orig_feat)
      validation[, g := g] 
      # Find optimal aim portfolio 
      aims <- pfml_input |> pfml_aims_fun(validation=validation, data_tc=data_tc, hp_coef=pfml_hp_coef, hp_years=hp_years, dates_oos=dates_oos, l_vec=l_vec, orig_feat=orig_feat)
      print(paste0("g: ", formatC(g, digits = 2, format="f"), " took ", formatC((proc.time()[3]-start_g[3])/60, digits = 2, format = "f"), " min"))
      # Output
      list("aim_pfs_list"=aims, "validation"=validation, "rff_w"=rff_w)
    })
    names(hps) <- g_vec
    proc.time()-start_time
  }
  # Implement final portfolio -----------------
  best_hps <- hps %>% lapply(function(x) x$validation) %>% rbindlist() %>% select(-rank)
  best_hps[, rank := frank(-cum_obj), by = .(eom_ret)]
  best_hps <- best_hps[rank==1 & month(eom_ret)==12]
  # Check that hp range is appropriate
  print(best_hps %>% pivot_longer(c(p, l, g)) %>% ggplot(aes(eom_ret, value)) + geom_point(alpha=0.5) + geom_line() + facet_wrap(~name, scales = "free_y", ncol=1))
  # Aim portfolios
  best_hps_list <- dates_oos %>% lapply(function(d) {
    d_ret <- d+1+months(1)-1
    oos_year <- year(d_ret)
    best_g <- best_hps[year(eom_ret)==oos_year-1]$g
    best_p <- best_hps[year(eom_ret)==oos_year-1]$p
    best_aim <- hps[[as.character(best_g)]]$aim_pfs_list[[as.character(d)]]$aim_pf
    best_coef <- hps[[as.character(best_g)]]$aim_pfs_list[[as.character(d)]]$coef
    # Output
    list("g"=best_g, "p"=best_p, "aim"=best_aim, "coef"=best_coef)
  }) 
  names(best_hps_list) <- dates_oos
  aims <- best_hps_list %>% map("aim") %>% rbindlist()
  # Final portfolio
  w <- data_tc[eom %in% dates_oos & valid==T][, .(id, eom, eom_ret, me, tr_ld1, valid)] %>% 
    pfml_w(dates = dates_oos, cov_list=cov_list, lambda_list=lambda_list, gamma_rel=gamma_rel, iter=iter, risk_free=risk_free, wealth=wealth, mu=mu, aims = aims)
  pf <- w %>% pf_ts_fun(data=data_tc, wealth = wealth, gam = gamma_rel) %>% mutate(type = "Portfolio-ML")
  # List of Ws used
  rff_w_list <- hps %>% map("rff_w")
  names(rff_w_list) <- g_vec
  # Output
  list("hps"=hps, "best_hps"=best_hps, "best_hps_list"=best_hps_list, "aims"=aims, "w"=w, "pf"=pf, "rff_w_list"=rff_w_list)
}

# Function: Portfolio-ML with counterfactual inputs ----------
pfml_cf_fun <- function(data, cf_cluster, pfml_base, dates, cov_list, scale, orig_feat, gamma_rel, wealth, risk_free, mu, iter, seed) {
  # Build scale vector, if required
  if (scale) {
    scales <- dates %>% lapply(function(d) {
      sigma <- cov_list[[as.character(d)]] |> create_cov()
      diag_vol <- sqrt(diag(sigma))
      data.table(id=as.numeric(rownames(sigma)), eom=d, vol_scale=diag_vol)
    }) |> rbindlist()
    # Add scales to data
    data <- scales[data, on=.(id, eom)]
    # Impute missing sigmas with cross-sectional mean
    data[, vol_scale := if_else(is.na(vol_scale), median(vol_scale, na.rm=T), vol_scale), by = eom]
  } 
  # Counterfactual aim portfolio
  set.seed(seed) # Ensures reproducability and that the same firms are shuffled for easier comparison across clusters
  # Counterfactual data
  cf <- data[, c("id", "eom", "vol_scale", features), with=F] # Counterfactual data
  if (cf_cluster != "bm") {
    cf[, id_shuffle := sample(id, .N, replace=F), by = eom]
    chars_sub <- cluster_labels[cluster==cf_cluster & characteristic %in% features, characteristic]
    chars_data <- cf %>% select(all_of(c("id", "eom", chars_sub))) %>% rename("id_shuffle"=id)
    cf <- chars_data[cf[, (chars_sub) := NULL], on = .(id_shuffle, eom)]
  }
  # Counterfactual aim
  aim_cf <- as.character(dates) %>% lapply(function(d) {
    stocks <- cf[eom == as.Date(d), id]
    # Use base hyper-parameters/aim coefficients
    best_g <- pfml_base$best_hps_list[[d]]$g
    best_p <- pfml_base$best_hps_list[[d]]$p
    aim_coef <- pfml_base$best_hps_list[[d]]$coef
    W <- pfml_base$hps[[as.character(best_g)]]$rff_w[, 1:(best_p/2)] # Plus 1 to include constant
    # Counterfactual input
    rff_x <- cf[eom == as.Date(d), features, with=F] %>% rff(W = W)
    # Counterfactual Signal
    s <- cbind(rff_x$X_cos, rff_x$X_sin)
    colnames(s) <- c(paste0("rff", 1:(best_p/2), "_cos"), paste0("rff", 1:(best_p/2), "_sin"))
    s <- s %>% apply(2, function(x) x-mean(x))            # De-mean
    s <- s %>% cbind(constant=rep(1, nrow(s)))
    s <- s %>% apply(2, function(x) x*sqrt((1/sum(x^2)))) # Scale
    # Reorder to fit with aim_coefficients
    feat <- pfml_feat_fun(p = best_p, orig_feat = orig_feat)
    s <- s[, feat]
    if(scale) {
      scales <- (1/cf[eom == as.Date(d)]$vol_scale) |> diag()
      s <- scales %*% s
    }
    # Counterfactual aim portfolio
    cf[eom==as.Date(d), .(id, eom, w_aim = drop(s %*% aim_coef))]
  }) %>% rbindlist()
  # Counterfactual portfolio
  w_cf <- data[eom %in% dates_oos & valid==T][, .(id, eom, eom_ret, me, tr_ld1, valid)] %>% 
    pfml_w(dates = dates, cov_list=cov_list, lambda_list=lambda_list, gamma_rel=gamma_rel, iter=iter, risk_free=risk_free, wealth=wealth, mu=mu, aims = aim_cf)
  pf_cf <- w_cf %>% pf_ts_fun(data=data, wealth = wealth, gam = gamma_rel) %>% mutate(type = "Portfolio-ML")
  # Output
  pf_cf |> mutate(cluster = cf_cluster)
}

# Multiperiod-ML ---------------------------------------------
# Multiperiod-ML aim portfolio 
# Works for any return prediction horizon# K=return horizon (in paper, appendix A.2 it's return horizon-1)
mp_aim_fun <- function(preds, sigma_gam, lambda, m, rf, mu, w, K) {
  id <- diag(nrow(sigma_gam))
  sigma_inv <- solve(sigma_gam)
  # g_bar <-  diag(drop(matrix(1+er+rf, ncol=1) / (1+rf+mu)))
  g_bar <- diag(nrow(lambda))
  m_gbar <- m %*% g_bar 
  c <- w^-1*m %*% solve(lambda) %*% sigma_gam # Are we missing g_bar?
  # c <- (id - m) %^% 2
  c_tilde <- (solve(id - m_gbar)%^%2) %*% c   # Causes massive issues if m isn't exactly right!! Check summary(diag(c_tilde)) should be VERY close to I if m_gbar=1
  c_tilde_sigma_inv <- c_tilde %*% sigma_inv
  aim_pf <- 0
  m_gbar_pow <- diag(nrow(m_gbar))  # Same as m_gbar %^% 0
  for (tau in 1:K) {
    if(tau>1) m_gbar_pow <- m_gbar_pow %*% m_gbar
    aim_pf <- aim_pf + m_gbar_pow %*% c_tilde_sigma_inv %*% preds[[paste0("pred_ld", tau)]]
  }
  # Re-scale
  solve(id-m) %*% ((id - m_gbar)%^%2) %*% solve(id - m_gbar_pow%*%m_gbar) %*% aim_pf 
}

# Multiperiod-ML validation
mp_val_fun <- function(data, dates, cov_list, lambda_list, wealth, risk_free, mu, gamma_rel, cov_type, iter, K, k=NULL, g=NULL, u_vec=NULL, hps=NULL, print=T) {
  # Initiate list of weights for each mu
  w_init <- data %>% initial_weights_new(w_type = "vw")
  w_init <- data[, .(id, eom, tr_ld1)][w_init, on = .(id, eom)]
  w_init <- wealth[, .(eom, mu_ld1)][w_init, on = "eom"]
  if (!is.null(hps)) {
    w_list <- list("opt"=w_init)
  } else {
    w_list <- replicate(length(u_vec), w_init, simplify = FALSE)
    names(w_list) <- u_vec
  }
  # Update weights recursively
  for (d in as.character(dates)) {
    if (print & year(as.Date(d))%%10==0 & month(as.Date(d))==12) print(paste("   ", d))
    # If given HPs use those
    if (!is.null(hps)) {
      hp <- hps[year(eom_ret)<year(as.Date(d))][eom_ret == max(eom_ret)]
      g <- hp$g; u <- hp$u; k <- hp$k
    }
    # Inputs
    wealth_t <- wealth[eom == as.Date(d)]$wealth
    rf <- risk_free[eom==as.Date(d), rf]
    data_d <- data[eom==as.Date(d)]
    ids <- data_d$id
    sigma_gam <- (cov_list[[d]] |> create_cov(ids = as.character(ids)))*gamma_rel 
    sigma_gam <- sigma_gam |> sigma_gam_adj(g = g, cov_type=cov_type)
    lambda <- lambda_list[[d]] |> create_lambda(ids = as.character(ids))
    lambda <- lambda*k
    m <- m_func(w=wealth_t, mu=mu, rf=rf, sigma_gam = sigma_gam, gam=gamma_rel, lambda=lambda, iter=iter)
    iden <- diag(nrow(m))
    # Aim portfolio if u=1
    w_aim_one <- data_d %>% mp_aim_fun(sigma_gam = sigma_gam, lambda = lambda, m = m, rf = rf, mu = mu, w=wealth_t, K=K)
    # Actual portfolio for each mu
    for(u_nm in names(w_list)) {
      if(is.null(hps)) u <- as.numeric(u_nm)
      fa_aims <- data_d[, .(id, eom, w_aim=drop(w_aim_one)*u)]
      # Build optimal portfolio
      w_cur <- fa_aims[w_list[[u_nm]][eom==as.Date(d)], on = .(id, eom)]
      w_cur$w_opt <- m %*% w_cur$w_start + (iden-m) %*% w_cur$w_aim
      # Update weight matrix
      next_month <- dates[match(as.Date(d), dates)+1]
      w_list[[u_nm]] <- w_cur[, .(id, w_opt, w_opt_ld1 = w_opt*(1+tr_ld1)/(1+mu_ld1))][w_list[[u_nm]], on= .(id)]
      w_list[[u_nm]][!is.na(w_opt) & eom==as.Date(d), w := w_opt] # This months optimal weights
      w_list[[u_nm]][!is.na(w_opt) & eom==next_month, w_start := w_opt_ld1] # Next months start weights
      w_list[[u_nm]][eom==next_month & is.na(w_start), w_start := 0] # New stocks are initialized at a zero weight
      w_list[[u_nm]][, c("w_opt", "w_opt_ld1") := NULL]
    }
  } 
  if (!is.null(hps)) {
    w_list <- w_list %>% rbindlist()
  }
  # Output
  return(w_list)
}

# Multiperiod-ML: Full implementation 
mp_implement <- function(data_tc, cov_list, lambda_list, rf, 
                         wealth, mu, gamma_rel,
                         dates_full, dates_oos, dates_hp, hp_years,
                         k_vec, u_vec, g_vec, cov_type, K,  
                         iter, validation=NULL, seed) {
  # Prepare HP grid
  mp_hps <- expand.grid(k=k_vec, u=u_vec, g=g_vec)
  # Relevant data 
  # data_rel <- data_tc[eom %in% dates_hp & valid==T, .(id, eom, me, tr_ld1, pred_ld1, pred_ld2_6, pred_ld7_12)][order(id, eom)]
  data_rel <- data_tc[eom %in% dates_hp & valid==T, c("id", "eom", "me", "tr_ld1", paste0("pred_ld", 1:K)), with=F][order(id, eom)]
  # Validation
  if (is.null(validation)) {
    mp_k_g <- expand.grid(k=k_vec, g=g_vec)
    validation <- 1:nrow(mp_k_g) %>% lapply(function(i) {
      print(paste0(i, " out of ", nrow(mp_k_g)))
      # Multiperiod-ML
      hp <- mp_k_g[i, ]
      mp_w_list <- data_rel %>% 
        mp_val_fun(dates = dates_hp, cov_list=cov_list, lambda_list=lambda_list, mu = mu, gamma_rel = gamma_rel, wealth=wealth, risk_free=rf, k = hp$k, g=hp$g, u_vec=u_vec, cov_type = cov_type, iter=iter, print=T, K=K)
      names(mp_w_list) %>% lapply(function(u_chr) {
        mp_w_list[[u_chr]] %>% pf_ts_fun(data=data_tc, wealth = wealth, gam = gamma_rel) %>% mutate(k=hp$k, g=hp$g, u=as.numeric(u_chr))
      }) %>% rbindlist()
    }) %>% rbindlist()
  }
  # Optimal hps
  validation %>% setorder(k, g, u, eom_ret)
  validation[, cum_var := cummean(r^2)-cummean(r)^2, by = .(k, g, u)]
  validation[, cum_obj := cummean(r-tc-0.5*cum_var*gamma_rel), by = .(k, g, u)]
  validation[, rank := frank(-cum_obj), by = .(eom_ret)]
  print(validation %>%
          mutate(name = paste0("u:", u, "k:", round(k, 2), "g:", g)) %>%
          filter(year(eom_ret)>=1981) %>%
          group_by(k, g, u) %>%
          mutate(min_rank = min(rank)) %>%
          filter(min_rank <=1) %>% 
          ggplot(aes(eom_ret, cum_obj*12, colour = name)) +
          geom_line())
  optimal_hps <- validation[month(eom_ret)==12 & rank==1][order(eom_ret)]
  # Implement final portfolio
  print(paste0("Final portfolio"))
  # w <- data_tc[eom %in% dates_oos & valid==T, .(id, eom, me, tr_ld1, pred_ld1, pred_ld2_6, pred_ld7_12)][order(id, eom)] %>% 
  w <- data_tc[eom %in% dates_oos & valid==T, c("id", "eom", "me", "tr_ld1", paste0("pred_ld", 1:K)), with=F][order(id, eom)] |> 
    mp_val_fun(dates = dates_oos, cov_list=cov_list, lambda_list=lambda_list, hps = optimal_hps, mu = mu, gamma_rel = gamma_rel, cov_type = cov_type, iter=iter, wealth=wealth, risk_free=rf, K=K)
  pf <- w %>% pf_ts_fun(data=data_tc, wealth = wealth, gam = gamma_rel) %>% mutate(type = "Multiperiod-ML*")
  # Output
  list("hps"=validation, "best_hps"=optimal_hps, "w"=w, "pf"=pf)
}


