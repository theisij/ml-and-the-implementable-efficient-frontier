# Libraries ---------------------------------------
library(tictoc)
library(lfe)
library(cowplot)
library(MASS)
library(xtable)
library(roll)
library(expm)
library(glmnet)
library(lubridate)
library(tidyverse)
library(data.table)
source("0 - General functions.R")
source("0 - Return prediction functions.R")
source("0 - Portfolio choice functions.R")
Rcpp::sourceCpp("ewma.cpp")
Rcpp::sourceCpp("sqrtm_cpp.cpp")
options(dplyr.summarise.inform = FALSE)

output <- list()

# Layout Settings ---------------
theme_set(theme_classic())
colours_theme <- c("#0C6291", "#A63446", RColorBrewer::brewer.pal(8, "Dark2"), 
                   "darkslategrey", "blue3", "red3", "purple2", "yellow2", "aquamarine",
                   "grey", "salmon", "antiquewhite", "chartreuse") 
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = colours_theme)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = colours_theme)
}

pf_order <- c("Portfolio-ML", 
              "Multiperiod-ML", "Multiperiod-ML*",
              "Static-ML", "Static-ML*", 
              "Market", "1/N", "Minimum Variance", "Factor-ML", "Rank-ML", "Markowitz-ML")
pf_order_new <-  c("Portfolio-ML", "Multiperiod-ML*", "Static-ML*", "Multiperiod-ML", "Static-ML", "Markowitz-ML", "Rank-ML", 
                   "Factor-ML", "Minimum Variance", "1/N", "Market")
main_types <- c("Portfolio-ML", "Multiperiod-ML*", "Static-ML*", "Factor-ML", "Markowitz-ML")
cluster_order <- c("Accruals", "Debt Issuance", "Investment", "Short-Term Reversal", "Value",
                   "Low Risk", "Quality", "Momentum", "Profitability", "Profit Growth",
                   "Seasonality", "Size", "Low Leverage")

txt_size <- 10

output_path_fig <- "Figures"
format <- "pdf"
fig_h <- 3.2
fig_w <- 6.5 # 6.5 is rouhghly the page width

# Setup -------------------------
# Data settings
settings <- list(
  parallel = T,  
  seed_no = 1,
  months = FALSE,
  split = list(
    train_end = as.Date("1970-12-31"), # Change to 1994-12-31?
    test_end = as.Date("2020-12-31"),
    val_years = 10,
    model_update_freq = "yearly", # In c("once", "yearly", "decade") 
    train_lookback = 1000, # Set to high number (e.g. 1000) if you want an expanding
    retrain_lookback = 1000 # Set to high number (e.g. 1000) if you want an expanding
  ),
  feat_prank = T,  # Percentile rank features?
  ret_impute = "zero", # Impute missing returns with
  feat_impute = T,
  addition_n = 12,     # Need to be valid for the past n months to be included in investment universe
  deletion_n = 12,     # Exclude from universe after N periods where the stocks is non-valid
  screens = list(
    start = as.Date("1952-12-31"), 
    end = as.Date("2020-12-31"),
    feat_pct = 0.5,
    nyse_stocks = T
  ),
  pi = 0.1,    # What is the price impact as a function of the daily trading volume? For example, if pi=0.1, then trading 1% of the daily dollar volume has an average impact of 0.1%
  rff = list(
    p_vec = 2^c(1:9),
    g_vec = exp(-3:-2),
    l_vec = c(0, exp(seq(-10, 10, length.out = 100))) # Before we also had exp(-100) but that's effectively zero 
  ),
  pf = list(
    dates = list(
      start_year = 1971,
      end_yr = 2020,
      split_years = 10
    ),
    hps = list(
      cov_type = "cov_add",
      m1 = list(
        k = c(1, 2, 3),
        u = c(0.25, 0.5, 1),
        g = c(0, 1, 2),
        K = 12
      ),
      # m2 = list(l = c(0, exp(-3:3))), # c(0, exp(-10:3))
      static = list(
        k = c(1/1, 1/3, 1/5),
        u = c(0.25, 0.5, 1),
        g = c(0, 1, 2)
      )
    )
  ),
  pf_ml = list( 
    g_vec = exp(-3:-2), 
    p_vec = c(2^(6:9)), 
    l_vec = c(0, exp(seq(-10, 10, length.out = 100))), # Vastly expanded the search set for lambda
    orig_feat = F,          # Should original features be added to the list of random features?
    scale = T
  ),
  ef = list(
    wealth = c(1, 1e9, 1e10, 1e11),
    gamma_rel = c(1, 5, 10, 20, 100)
  ),
  cov_set = list(
    industries = T,   # Should we include industry dummies in the covariance matrix? No feasible when we look across size groups (because not all induistries have observations) 
    obs = 252*10,    # Check tibble(t = 1:(settings$cov_set$obs), w = (0.5^(1/(settings$cov_set$hl_cor)))^t) %>% ggplot(aes(t, w)) + geom_point() + geom_hline(yintercept = 0)
    hl_cor = 252*3/2,  # Barra uses 48 months as half-life, seems high. https://www.alacra.com/alacra/help/barra_handbook_GEM.pdf
    hl_var = 252/2,
    hl_stock_var = 252/2,
    min_stock_obs = 252,
    initial_var_obs = 21*3
  ),
  factor_ml = list(
    n_pfs = 10
  )
)

set.seed(settings$seed_no)

# Portfolio settings -----------
pf_set <- list(
  wealth = 1e10,
  gamma_rel = 10,
  mu = 0.007, # How much do we expect the portfolio to grow each month? market[year(eom_ret)>=1980, mean(mkt_vw_exc, na.rm=T)]
  # aim_hor = max_t_hor-1,
  lb_hor = 11 # Change??
)

# Features ---------------------
features <- c(
  "age",                 "aliq_at",             "aliq_mat",            "ami_126d",           
  "at_be",               "at_gr1",              "at_me",               "at_turnover",        
  "be_gr1a",             "be_me",               "beta_60m",            "beta_dimson_21d",    
  "betabab_1260d",       "betadown_252d",       "bev_mev",             "bidaskhl_21d",       
  "capex_abn",           "capx_gr1",            "capx_gr2",            "capx_gr3",           
  "cash_at",             "chcsho_12m",          "coa_gr1a",            "col_gr1a",           
  "cop_at",              "cop_atl1",            "corr_1260d",          "coskew_21d",         
  "cowc_gr1a",           "dbnetis_at",          "debt_gr3",            "debt_me",            
  "dgp_dsale",           "div12m_me",           "dolvol_126d",         "dolvol_var_126d",    
  "dsale_dinv",          "dsale_drec",          "dsale_dsga",          "earnings_variability",
  "ebit_bev",            "ebit_sale",           "ebitda_mev",          "emp_gr1",            
  "eq_dur",              "eqnetis_at",          "eqnpo_12m",           "eqnpo_me",           
  "eqpo_me",             "f_score",             "fcf_me",              "fnl_gr1a",           
  "gp_at",               "gp_atl1",             "ival_me",             "inv_gr1",            
  "inv_gr1a",            "iskew_capm_21d",      "iskew_ff3_21d",       "iskew_hxz4_21d",     
  "ivol_capm_21d",       "ivol_capm_252d",      "ivol_ff3_21d",        "ivol_hxz4_21d",      
  "kz_index",            "lnoa_gr1a",           "lti_gr1a",            "market_equity",      
  "mispricing_mgmt",     "mispricing_perf",     "ncoa_gr1a",           "ncol_gr1a",          
  "netdebt_me",          "netis_at",            "nfna_gr1a",           "ni_ar1",             
  "ni_be",               "ni_inc8q",            "ni_ivol",             "ni_me",              
  "niq_at",              "niq_at_chg1",         "niq_be",              "niq_be_chg1",        
  "niq_su",              "nncoa_gr1a",          "noa_at",              "noa_gr1a",           
  "o_score",             "oaccruals_at",        "oaccruals_ni",        "ocf_at",             
  "ocf_at_chg1",         "ocf_me",              "ocfq_saleq_std",      "op_at",              
  "op_atl1",             "ope_be",              "ope_bel1",            "opex_at",            
  "pi_nix",              "ppeinv_gr1a",         "prc",                 "prc_highprc_252d",   
  "qmj",                 "qmj_growth",          "qmj_prof",            "qmj_safety",         
  "rd_me",               "rd_sale",             "rd5_at",              "resff3_12_1",        
  "resff3_6_1",          "ret_1_0",             "ret_12_1",            "ret_12_7",           
  "ret_3_1",             "ret_6_1",             "ret_60_12",           "ret_9_1",            
  "rmax1_21d",           "rmax5_21d",           "rmax5_rvol_21d",      "rskew_21d",          
  "rvol_21d",            "sale_bev",            "sale_emp_gr1",        "sale_gr1",           
  "sale_gr3",            "sale_me",             "saleq_gr1",           "saleq_su",           
  "seas_1_1an",          "seas_1_1na",          "seas_11_15an",        "seas_11_15na",       
  "seas_16_20an",        "seas_16_20na",        "seas_2_5an",          "seas_2_5na",         
  "seas_6_10an",         "seas_6_10na",         "sti_gr1a",            "taccruals_at",       
  "taccruals_ni",        "tangibility",         "tax_gr1a",            "turnover_126d",      
  "turnover_var_126d",   "z_score",             "zero_trades_126d",    "zero_trades_21d",    
  "zero_trades_252d",
  "rvol_252d"
)
# Exclude features without sufficient coverage
feat_excl <- c("capex_abn", "capx_gr2", "capx_gr3", "debt_gr3", "dgp_dsale",           
               "dsale_dinv", "dsale_drec", "dsale_dsga", "earnings_variability", "eqnetis_at",          
               "eqnpo_me", "eqpo_me", "f_score", "iskew_hxz4_21d", "ivol_hxz4_21d",       
               "netis_at", "ni_ar1", "ni_inc8q", "ni_ivol", "niq_at", "niq_at_chg1", "niq_be", 
               "niq_be_chg1", "niq_su", "ocfq_saleq_std", "qmj", "qmj_growth", "rd_me", 
               "rd_sale", "rd5_at", "resff3_12_1", "resff3_6_1", "sale_gr3", "saleq_gr1", 
               "saleq_su", "seas_16_20an", "seas_16_20na", "sti_gr1a", "z_score")
features <- features[!(features %in% feat_excl)]
