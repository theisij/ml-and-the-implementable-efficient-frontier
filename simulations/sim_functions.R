# Simulate data --------
sim_prep_data <- function(t, n, disp_x, tv_sds, tv_thetas, tv_rps, dolvol, dates_full, rf, seed, add_noise_feat=T, feat_standardize=T) {
  set.seed(seed)
  # Economy -------------------------------
  dt <- 1 # Time increment
  mu_x <- 1 # Long-term mean of x1
  rpx <- 0.05  # Annual risk premium of X
  n_tv <- length(tv_sds)
  features <- c("x", paste0("tv", 1:n_tv))
  
  # One constant factor that determines covariances 
  X <- rep(rnorm(n = n, mean = mu_x, sd = disp_x), t) |> matrix(nrow = n, ncol = t) |> t() 
  
  # Multiple time-varying return predicting factors, unrelated to covariances
  tv_list <- 1:n_tv |> map(function(k) {
    tv_start <- rnorm(n = n, mean = 0, sd = tv_sds[k])  # Initial value
    tv <- matrix(NA, nrow = t, ncol = n)
    tv[1,] <- tv_start
    for (j in 2:t) {
      tv[j,] <- tv[j-1,] + (1-tv_thetas[k]) * (0 - tv[j-1,]) * dt + tv_sds[k] * rnorm(n = n, mean = 0, sd = sqrt(dt))
    }
    return(tv)
  })
  
  # Simulate return --------------------
  sig_m <- 0.2
  sig_e <- 0.4
  ivol <- diag(sig_e^2/12, nrow=n)
  mkt_ret <- rnorm(n=t, mean=0, sd = sig_m/sqrt(12)) # Demeaned market return
  e_ret <- matrix(rnorm(n*t, sd = sig_e/sqrt(12)), ncol = n, nrow = t)
  data_tc <- 1:t |> map(function(j) {
    x <- X[j,]
    # Only x is related to sigma
    sigma <- x %*% as.matrix(sig_m^2/12) %*%  t(x) + ivol
    # Both x and TV factors are related to the expected return
    mu <- rpx/12*x
    for (k in 1:n_tv) {
      mu <- mu + tv_rps[k]/12*tv_list[[k]][j,]  
    }
    # Realized returns
    if (F) {
      r <- mvrnorm(n = 1, mu = mu, Sigma = sigma)
    } else {
      r <- mu + x*mkt_ret[j] + e_ret[j, ]
    }
    w_tpf <- drop(solve(sigma) %*% mu) 
    data <- data.table(eom=dates_full[j], id=1:n, x=x, ret_ld1 = r, er_true = mu, sr = mu/diag(sigma)^0.5, w_tpf=w_tpf)
    for (k in 1:n_tv) {
      data[, paste0("tv", k) := tv_list[[k]][j,]]
    }
    return(data)
  }) |> rbindlist()
  if (add_noise_feat) {
    noise_n <- 5
    for (i in 1:noise_n) {
      data_tc[, paste0("feat", i) := rnorm(t*n)]
    }
    features <- c(features, paste0("feat", 1:noise_n))
  }
  data_tc[, me := 1]
  
  # Lambda (consider it fixed for now) --
  data_tc[, lambda := 0.02/dolvol] # Assumes that all stocks have same daily trading volume
  # data_tc[, ret_ld1 := shift(r, 1, type="lead"), by=id]
  data_tc[, tr_ld1 := ret_ld1+rf]
  data_tc[, tr_ld0 := lag(tr_ld1, 1), by = id]
  data_tc[eom==min(eom), tr_ld0 := 0] # hack
  # data_tc[, pred_ld1 := 0.05/12]
  # data_tc[, pred_ld1 := er_true]
  for (h in 1:12) {
    # data_tc[, paste0("pred_ld", h) := rp1/12*(theta^(h-1)*x1+(1-theta^(h-1))*mu_x1) + rp2/12*(theta^(h-1)*x2+(1-theta^(h-1))*mu_x2)] # Risk premium times expected x:
    # data_tc[, paste0("pred_ld", h) := rp1/12*x1 + rp2/12*(theta^(h-1)*x2+(1-theta^(h-1))*mu_x2)] # Risk premium times expected x:
    data_tc[, paste0("pred_ld", h) := rpx/12*x]
    for (k in 1:n_tv) {
      data_tc[, paste0("pred_ld", h) := get(paste0("pred_ld", h)) + tv_rps[k]/12*(tv_thetas[k]^(h-1)*get(paste0("tv", k))+(1-tv_rps[k]^(h-1))*0)]
    }
  }
  print(data_tc[, c("id", "eom", paste0("pred_ld", 1:12)), with=F][id<=5 & eom==max(eom)] |> pivot_longer(-c(id, eom)) |> mutate(ld = str_remove(name, "pred_ld") |> as.integer()) |> ggplot(aes(ld, value, colour=factor(id))) + geom_point() + geom_line() + 
          labs(title=paste0("Expected return for ID 1:5 on last date, tv thetas: ", paste0(tv_thetas, collapse="-"))))
  # Prepare for multiperiod-ML
  data_tc[, pred_ld2_6 := rowMeans(.SD), .SDcols = paste0("pred_ld", 2:6)]
  data_tc[, pred_ld7_12 := rowMeans(.SD), .SDcols = paste0("pred_ld", 7:12)]
  if (FALSE) {
    data_tc[id<=2, c("id", "eom", paste0("pred_ld", 1:4))][year(eom) %in% 2020:2021] |> pivot_longer(-c(id,eom)) |> ggplot(aes(eom, value, colour=name)) + geom_line() + facet_wrap(~id) + geom_hline(yintercept = rpx/12)
    data_tc[id<=2, c("id", "eom", "pred_ld1", "pred_ld2_6", "pred_ld7_12")][year(eom) %in% 2020:2021] |> pivot_longer(-c(id,eom)) |> ggplot(aes(eom, value, colour=name)) + geom_line() + facet_wrap(~id) + geom_hline(yintercept = rpx/12)
  }
  # Standardize features
  if (feat_standardize) {
    for(f in features) {
      data_tc[, (f) := ecdf(get(f))(get(f)), by = eom]
    }
  }
  # For existing functions 
  data_tc[, valid := T]
  data_tc[, mu_ld0 := 0]  #pf_set$mu
  data_tc[, eom_ret := eom+1+months(1)-1]
  # Create list of sigmas 
  barra_cov <- 1:t |> map(function(i) {
    fct_load <- as.matrix(X[i,])
    rownames(fct_load) <- as.character(1:n)
    ivol_vec <- diag(ivol)
    names(ivol_vec) <- as.character(1:n)
    fct_cov <- as.matrix(sig_m^2/12)
    list("fct_load"=fct_load, "fct_cov"=fct_cov, "ivol_vec"=ivol_vec)
  })
  names(barra_cov) <- dates_full
  # Output
  list("barra_cov"=barra_cov, "data"=data_tc, "features"=features)
}
