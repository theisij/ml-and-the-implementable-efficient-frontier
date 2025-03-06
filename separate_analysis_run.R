# Load packages and settings ---------------------------------------------------
source("Main.R")

get_from_path_model <- "Data/Generated/Models/20240424-all"  # From which directory should the data be loaded? (if update$x==F)
run_sub <- F  # T when testing code, F otherwise

# Screens for main sample ------------------------------------------------------
settings$screens$size_screen <- "perc_low50_high100_min50"
pf_set$wealth <- 1e10
pf_set$gamma_rel <- 10

# Generate data for analysis ---------------------------------------------------
source("1 - Prepare Data.R", echo = T)
source("3 - Estimate Covariance Matrix.R", echo = T)
source("4 - Prepare Portfolio Data.R", echo = T)

# Create benchmark portfolio objects -------------------------------------------
# This repeats some of the data in 5 - Base case.R, without re-doing the computationally heavy parts 
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

# Generate results from analysis -----------------------------------------------
source("6 - Implementable efficient frontier.R", echo = T) 
source("6 - Base analysis.R", echo = T)
source("6 - Performance across size distribution.R", echo = T)
source("6 - Feature Importance.R", echo = T)
source("6 - Economic intuition.R", echo = T)
source("6 - Short selling fees.R", echo = T)
source("6 - RF Example.R", echo = T)
