# This file relative to simulation.R: Run for 1, rather than multiple seeds
# TO DO
# - Prepare portfolio-ML for real data
library(MASS)
library(expm)
library(tidyverse)
library(data.table)
source("0 - General functions.R")
source("0 - Return prediction functions.R")
source("0 - Portfolio choice functions.R")
Rcpp::sourceCpp("sqrtm_cpp.cpp")
# Goal: Understand the performance of the different methods with a fixed DGP
# Setup: One-factor that drives expected returns and covariances
# Result: Evaluation of how well Static/Portfolio/multiperiod performs

add_noise_feat <- T

source("simulations/sim_functions.R")
# source("simulations/m2_fun_new_2.R")
# source("simulations/new_static_fun.R")
# source("simulations/new_multi_fun.R")
# source("simulations/memory_fun_new.R")

# Submit a config file with slurm job? -----------
if (TRUE) {
  # Prepare for slurm to submit configuration files flexibly
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- args[1]  # Get the first argument, which is the config file name
  
  config_params <- read_config(config_file)
} else {
  config_params <- list(
    "seed" = 14
  )
}

# Settings -------
set <- list(
  seed_no = 1,
  split = list(
    train_end = as.Date("1970-12-31"), # Change to 1994-12-31?
    test_end = as.Date("2020-12-31"),
    val_years = 10
  ),
  pf = list(
    dates = list(
      start_year = 1971,
      end_yr = 2020,
      split_years = 10
    ),
    hps = list(
      cov_type = "cov_add",
      static = list(
        k = c(1/1, 1/6, 1/12),
        u = c(1),
        g = c(0)
      ),
      m1 = list(
        k = c(1),
        u = c(1),
        g = c(0)  
      )
    )
  ),
  pf_ml = list( 
    g_vec = exp(-3:-2), 
    p_vec = c(2^(6:9)), 
    l_vec = c(0, exp(seq(-10, 10, length.out = 100))), 
    orig_feat = F,          # Should original features be added to the list of random features?
    scale = T
  )
)

t <- 12*70

# Important dates ----------------------
dates_full <- rev(set$split$test_end+1 - months(1:t))-1

hp_years <- seq(from=set$pf$dates$start_year, to=set$pf$dates$end_yr)
start_oos <- set$pf$dates$start_year+set$pf$dates$split_years

dates_oos <- seq.Date(from = as.Date(paste0(start_oos, "-", "01-01")), to = set$split$test_end+1-months(1), by = "1 month")-1
dates_hp <- seq.Date(from = as.Date(paste0(min(hp_years), "-", "01-01")), to = set$split$test_end+1-months(1), by = "1 month")-1

# Investor settings ------------------
pf_set <- list(
  # wealth = 1e11,
  gamma_rel = 10,
  mu = 0, # How much do we expect the portfolio to grow each month? market[year(eom_ret)>=1980, mean(mkt_vw_exc, na.rm=T)]
  # aim_hor = max_t_hor-1,
  lb_hor = 11 # Change??
)

# Simulation -------------------------------------
set$n_stocks <- 500
search_grid <- expand.grid(
  w = 10^(8:12),
  theta1 = c(0, 0.95),
  theta2 = c(0, 0.95),
  n = set$n_stocks
) |> filter(!(theta1==0 & theta2==0.95))  # Effectively the same as theta1=0.95, theta2=0, so no need to run it

do_static <- T
do_tpf <- T
do_mp <- T
do_pf <- T

# Takes 2-6 hours with 500 stocks, memory utilized is 7.54 GB
# With 3 repetitions and 500 stocks, I suggest requesting 15GB for 23 hours 
#    - It took between 8 and 14 hours with 32 CPUs
search_params <- search_grid[config_params$seed, ]
tictoc::tic(paste0("Run with parameters: wealth=", search_params$w, ", theta1=", search_params$theta1, ", theta2=", search_params$theta2, ", n=", search_params$n))
results <- 1:3 |> map(function(seed) {
  rf <- 0
  risk_free <- data.table(eom=dates_full, rf=rf)
  # Inputs to prepare_data_list ----
  wealth <- data.table(
    eom=dates_full, 
    wealth=search_params$w, 
    mu_ld1 = pf_set$mu)
  # Prepare data 
  system.time(data_list <- sim_prep_data(
    t = t,
    n = search_params$n,
    dates_full = dates_full, 
    dolvol = 84*1e6, # Median dolvol for valid stocks by 2020
    rf = rf,
    tv_sds = rep(0.25, 2),
    tv_thetas = c(search_params$theta1, search_params$theta2),
    tv_rps = rep(0.05, 2),
    disp_x = 0.5,
    seed = seed, 
    add_noise_feat = add_noise_feat,
    feat_standardize = T
  ))
  lambda_dates <- unique(data_list$data$eom)
  lambda_list <- lambda_dates |> map(function(d) {
    x <- data_list$data[eom==d, .(id, lambda)][order(id)]
    ids <- x$id
    x <- x$lambda
    names(x) <- ids
    return(x)
  }) |> setNames(as.character(lambda_dates))
  pfs <- data.table()
  if (do_static) {
    # Static portfolio
    static <- static_implement(
      data_tc = data_list$data, 
      cov_list = data_list$barra_cov, 
      lambda_list = lambda_list,
      rf = risk_free,                                      # Data
      wealth = wealth, 
      mu = pf_set$mu, 
      gamma_rel = pf_set$gamma_rel,                                                       # Investor
      dates_full = dates_full, 
      dates_oos = dates_oos, 
      dates_hp = dates_hp, 
      hp_years = hp_years,                               # Dates
      k_vec = set$pf$hps$static$k, 
      u_vec = set$pf$hps$static$u, 
      g_vec = set$pf$hps$static$g, 
      cov_type = set$pf$hps$cov_type,
      validation = NULL
    )
    static_raw <- static$hps[eom_ret %in% static$pf$eom_ret & k==1 & g==0 & u==1, .(eom_ret=as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Static-ML"]
    pfs <- pfs |> rbind(static$pf, static_raw)
  }
  if (do_tpf) {
    # TPF
    tpf <- data_list$data |> tpf_implement(cov_list = data_list$barra_cov, wealth = wealth, dates = dates_oos, gam = pf_set$gamma_rel)
    pfs <- pfs |> rbind(tpf$pf)
  }
  if (do_mp) {
    # Multiperiod-ML 
    m1 <- mp_implement(
      data_tc = data_list$data, 
      cov_list = data_list$barra_cov,
      lambda_list = lambda_list,
      rf = risk_free,                                      # Data
      wealth = wealth, 
      mu = pf_set$mu, 
      gamma_rel = pf_set$gamma_rel,                                                       # Investor
      dates_full = dates_full, 
      dates_oos = dates_oos, 
      dates_hp = dates_hp, 
      hp_years = hp_years,                              # Dates
      k_vec = set$pf$hps$m1$k, 
      u_vec = set$pf$hps$m1$u, 
      g_vec = set$pf$hps$m1$g, 
      cov_type = set$pf$hps$cov_type,
      validation = NULL, 
      iter = 10,
      K = 12
    )
    pfs <- pfs |> rbind(m1$pf)
  }
  if (do_pf) {
    m2 <- pfml_implement(
      data_tc = data_list$data, 
      cov_list = data_list$barra_cov, 
      lambda_list = lambda_list,
      features = data_list$features, 
      risk_free = risk_free,                                      # Data
      wealth = wealth,
      mu = pf_set$mu, 
      gamma_rel = pf_set$gamma_rel,                                                       # Investor
      dates_full = dates_full[13:length(dates_full)], 
      dates_oos = dates_oos, 
      lb = pf_set$lb_hor, 
      hp_years = hp_years,                               # Dates
      rff_feat = T, 
      g_vec = set$pf_ml$g_vec, 
      p_vec = set$pf_ml$p_vec, 
      l_vec = set$pf_ml$l_vec, 
      scale = set$pf_ml$scale, 
      orig_feat = set$pf_ml$orig_feat,      # Hyperparameters
      iter = 10, 
      hps = NULL,  
      balanced = T,                         # With 10 stocks, setting balanced=T increases time from 1.5 to 3mins. But with 100 stocks, it made almost no difference? Wierd
      seed = set$seed_no                                                      # Other
    )
    pfs <- pfs |> rbind(m2$pf)
  }
  # Output
  pfs |> mutate(seed=seed, w=search_params$w, n=search_params$n, theta1=search_params$theta1, theta2=search_params$theta2)
}, .progress = paste0("Progress run ", config_params$seed)) |> rbindlist() |> mutate(run=config_params$seed)
tictoc::toc()
if (T) {
  results |> fwrite(paste0("simulations/results/results", Sys.Date(), 
                           "_run", config_params$seed, 
                           "_nstocks", set$n_stocks, ".csv"))
}