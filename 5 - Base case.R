# Benchmark portfolios ------------------------------------------
# Markowitz-ML
tpf <- chars |> tpf_implement(cov_list = barra_cov, wealth = wealth, dates = dates_oos, gam = pf_set$gamma_rel)

# Factor-ML
factor_ml <- chars %>% factor_ml_implement(dates = dates_oos, n_pfs = settings$factor_ml$n_pfs, wealth = wealth, gam = pf_set$gamma_rel)

# Market
mkt <- chars %>% mkt_implement(dates = dates_oos, wealth = wealth)

# 1/n 
ew <- chars %>% ew_implement(dates = dates_oos, wealth = wealth)

# Fama-MacBeth / Rank weighted portfolios 
rw <- chars %>% rw_implement(dates = dates_oos, wealth = wealth)

# Minimum variance 
mv <- chars %>% mv_implement(cov_list = barra_cov, dates = dates_oos, wealth = wealth)

# Output
bm_pfs <- rbind(tpf$pf, factor_ml$pf, ew$pf, mkt$pf, rw$pf, mv$pf)
bm_pfs %>% fwrite(paste0(output_path, "/bms.csv"))

# Static-ML ------------------------------------------------------
# Implement
static <- static_implement(
  data_tc = chars, 
  cov_list = barra_cov, 
  lambda_list = lambda_list,
  rf = risk_free,                                      # Data
  wealth = wealth, 
  mu = pf_set$mu, 
  gamma_rel = pf_set$gamma_rel,                                                       # Investor
  dates_full = dates_m1, 
  dates_oos = dates_oos, 
  dates_hp = dates_hp, 
  hp_years = hp_years,                               # Dates
  k_vec = settings$pf$hps$static$k, 
  u_vec = settings$pf$hps$static$u, 
  g_vec = settings$pf$hps$static$g, 
  cov_type = settings$pf$hps$cov_type,
  validation = NULL
)
# Output 
static |> saveRDS(paste0(output_path, "/static-ml.RDS"))

# Portfolio-ML ---------------------------------------------------
# Implement
pfml <- pfml_implement(
  data_tc = chars, 
  cov_list = barra_cov, 
  lambda_list = lambda_list,
  features = features, 
  risk_free = risk_free,                                      # Data
  wealth = wealth,
  mu = pf_set$mu, 
  gamma_rel = pf_set$gamma_rel,                                                       # Investor
  dates_full = dates_m2, 
  dates_oos = dates_oos, 
  lb = pf_set$lb_hor, 
  hp_years = hp_years,                               # Dates
  rff_feat = T, 
  g_vec = settings$pf_ml$g_vec, 
  p_vec = settings$pf_ml$p_vec, 
  l_vec = settings$pf_ml$l_vec, 
  scale = settings$pf_ml$scale, 
  orig_feat = settings$pf_ml$orig_feat,      # Hyperparameters
  iter = 10, 
  hps = NULL,  
  balanced = F,
  seed = settings$seed_no                                                      # Other
)
# Output
pfml |> saveRDS(paste0(output_path, "/portfolio-ml.RDS"))

# Multiperiod-ML ------------------------------------------------
if (config_params$update_mp) {
  # Implement
  mp <- mp_implement(
    data_tc = chars, 
    cov_list = barra_cov,
    lambda_list = lambda_list,
    rf = risk_free,                                      # Data
    wealth = wealth, 
    mu = pf_set$mu, 
    gamma_rel = pf_set$gamma_rel,                                                       # Investor
    dates_full = dates_m1, 
    dates_oos = dates_oos, 
    dates_hp = dates_hp, 
    hp_years = hp_years,                              # Dates
    k_vec = settings$pf$hps$m1$k, 
    u_vec = settings$pf$hps$m1$u, 
    g_vec = settings$pf$hps$m1$g, 
    cov_type = settings$pf$hps$cov_type,
    validation = NULL, 
    iter = 10,
    K = settings$pf$hps$m1$K
  )
  # Save output
  mp |> saveRDS(paste0(output_path, "/multiperiod-ml.RDS"))
} 