# Risk-free rate --------------------
risk_free <- fread("Data/ff3_m.csv", select = c("yyyymm", "RF")) %>% mutate(rf=RF/100, eom = paste0(yyyymm, "01") %>% as.Date("%Y%m%d") %>% ceiling_date(unit="month")-1) %>% select(eom, rf)

# Market -----------------------
market <- fread("Data/market_returns.csv", colClasses = c("eom"="character"))
market <- market[excntry == "USA", .(eom_ret = as.Date(eom, format="%Y%m%d"), mkt_vw_exc)]

# Wealth: Assumed portfolio growth --------------------
wealth_func <- function(wealth_end, end, market, risk_free) {
  wealth <- risk_free[, .("eom_ret"=eom, rf)][market, on = .(eom_ret)][, tret := mkt_vw_exc+rf]
  wealth <- wealth[eom_ret <= end]
  wealth <- wealth[order(-eom_ret)][, wealth := cumprod(1-tret)*wealth_end]
  wealth[, .(eom = floor_date(eom_ret, unit = "month")-1, wealth, mu_ld1 = tret)] %>%
    rbind(data.table(eom=end, wealth = wealth_end, mu_ld1 = NA_real_)) %>%
    arrange(eom)
}
wealth <- wealth_func(wealth_end = pf_set$wealth, end = settings$split$test_end, market = market, risk_free = risk_free)

if (FALSE) {
  wealth %>% ggplot(aes(eom, wealth)) + geom_point() + scale_y_log10()
  chars[!is.na(dolvol), .(id, eom, dolvol, me)][, .(vw = sum(dolvol*me)/sum(me), ew = mean(dolvol), median = median(dolvol)), by = eom] %>% ggplot(aes(eom, vw)) + geom_point() + scale_y_log10()
}

# Cluster Labels (for covariance estimation) -------------
cluster_labels <- fread("Data/Cluster Labels.csv")
cluster_labels[, cluster := cluster %>% str_to_lower() %>% str_replace_all("\\s|-", "_")]
factor_signs <- readxl::read_xlsx("Data/Factor Details.xlsx") %>%
  select("characteristic"=abr_jkp, direction) %>%
  filter(!is.na(characteristic)) %>%
  mutate(direction=direction %>% as.numeric) %>%
  setDT()
cluster_labels <- factor_signs[cluster_labels, on = "characteristic"]
cluster_labels <- cluster_labels %>% rbind(data.table(characteristic="rvol_252d", cluster="low_risk", direction=-1)) # Assign rvol_252d to Low Risk

# Prepare matrix of future monthly returns ---------------
monthly <- fread("Data/world_ret_monthly.csv", select = c("excntry", "id", "eom", "ret_exc"), colClasses = c("eom"="character"))
monthly <- monthly[excntry == "USA" & id<=99999] # Only CRSP observations
monthly[, eom := eom %>% fast_strptime(format = "%Y%m%d") %>% as.Date()]
data_ret <- monthly %>% long_horizon_ret(h = settings$pf$hps$m1$K, impute = "zero")
data_ret_ld1 <- data_ret[, .(id, eom, eom_ret = eom+1+months(1)-1, ret_ld1)]
data_ret_ld1 <- risk_free[data_ret_ld1, on = "eom"]
data_ret_ld1[, tr_ld1 := ret_ld1 + rf] # Total return
data_ret_ld1[, rf := NULL]
# Add total return at t-1
data_ret_ld1 <- data_ret_ld1[, .(id, eom=eom+1+months(1)-1, "tr_ld0" = tr_ld1)][data_ret_ld1, on = .(id, eom)]
rm(monthly)

# Prepare data -----------------------------------
chars <- fread(paste0("Data/usa.csv"), 
               select = unique(c("id", "eom", "sic", "ff49", "size_grp", "me", "crsp_exchcd", "rvol_252d", "dolvol_126d", features)),  
               colClasses = c("eom" = "character", "sic"="character"))
chars <- chars[id <= 99999] # Only CRSP observations
chars[, eom := eom %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
# Add useful columns ------
chars[, dolvol := dolvol_126d] 
chars[, lambda := 2/dolvol*settings$pi]
chars[, rvol_m := rvol_252d * sqrt(21)]
# Add total return data 
chars <- data_ret_ld1[chars, on=.(id, eom)]
# Add wealth change
chars <- wealth[, .(eom=eom+1+months(1)-1, "mu_ld0" = mu_ld1)][chars, on = .(eom)]
# Screens ----
# Exchange code screen
if (settings$screens$nyse_stocks) {
  print(paste0("   NYSE stock screen excludes ", round(mean(chars$crsp_exchcd != 1) * 100, 2), "% of the observations"))
  chars <- chars[crsp_exchcd==1]
}
# Date screen
print(paste0("   Date screen excludes ", round(mean(chars$eom < settings$screens$start | chars$eom > settings$screens$end) * 100, 2), "% of the observations"))
chars <- chars[eom >= settings$screens$start & eom <= settings$screens$end]
# Monitor screen impact
n_start <- nrow(chars)
me_start <- sum(chars$me, na.rm = T)
# Require me
print(paste0("   Non-missing me excludes ", round(mean(is.na(chars$me)) * 100, 2), "% of the observations"))
chars <- chars[!is.na(me)]
# Require non-missing return t and t+1
print(paste0("   Valid return req excludes ", round(chars[, mean(is.na(tr_ld1) | is.na(tr_ld0))] * 100, 2), "% of the observations"))
chars <- chars[!is.na(tr_ld0) & !is.na(tr_ld1)]
# Require dolvol
print(paste0("   Non-missing/non-zero dolvol excludes ", round(mean(is.na(chars$dolvol) | chars$dolvol==0) * 100, 2), "% of the observations"))
chars <- chars[!is.na(dolvol) & dolvol > 0]
# Require stock to have SIC code (for covariance estimation with industry) 
print(paste0("   Valid SIC code excludes ", round(mean(chars$sic=="") * 100, 2), "% of the observations"))
chars <- chars[!is.na(sic)]

# Feature screens
feat_available <- chars %>% select(all_of(features)) %>% apply(1, function(x) sum(!is.na(x)))
min_feat <- floor(length(features)*settings$screens$feat_pct)
print(paste0("   At least ", settings$screens$feat_pct*100, "% of feature excludes ", round(mean(feat_available < min_feat)*100, 2), "% of the observations"))
chars <- chars[feat_available >= min_feat]
# Summary
print(paste0("   In total, the final dataset has ", round( (nrow(chars) / n_start)*100, 2), "% of the observations and ", round((sum(chars$me) / me_start)*100, 2), "% of the market cap in the post ", settings$screens$start, " data"))
# Screen out if running subset --------------------------------
if (run_sub) {
  set.seed(settings$seed)
  chars <- chars[id %in% sample(unique(chars$id), 2500, replace = F)] 
}
# Feature standardization -----------------------------
if (settings$feat_prank) {
  chars[, (features) := lapply(.SD, as.double), .SDcols = features]  # Convert feature columns to double to avoid loosing precision 
  for(f in features) {
    if (match(f, features) %% 10 == 0) print(paste0("Feature ", match(f, features), " out of ", length(features)))
    chars[, zero := (get(f) == 0)]
    chars[!is.na(get(f)), (f) := ecdf(get(f))(get(f)), by = eom] # Didn't have by statement before!!
    chars[zero == T, (f) := 0][, zero := NULL]  # Set exact zeros to 0 (ecdf always returns >0)
  }
} 
# Feature Imputation ----------------------------------
if (settings$feat_impute) {
  if (settings$feat_prank) {
    chars[, (features) := lapply(.SD, function(x) if_else(is.na(x), 0.5, x)), .SDcols=features]
  } else {
    chars[, (features) := lapply(.SD, function(x) if_else(is.na(x), median(x, na.rm=T), x)), .SDcols=features, by=eom]
  }
}
# Industry classification -----------------------------
chars[, ff12 := case_when(
  sic %in% c(100:999, 2000:2399, 2700:2749, 2770:2799, 3100:3199, 3940:3989) ~ "NoDur",
  sic %in% c(2500:2519, 3630:3659, 3710:3711, 3714, 3716, 3750:3751, 3792, 3900:3939, 3990:3999) ~ "Durbl",
  sic %in% c(2520:2589, 2600:2699, 2750:2769, 3000:3099, 3200:3569, 3580:3629,
             3700:3709, 3712:3713, 3715, 3717:3749, 3752:3791, 3793:3799, 3830:3839, 3860:3899) ~ "Manuf",
  sic %in% c(1200:1399, 2900:2999) ~ "Enrgy",
  sic %in% c(2800:2829, 2840:2899) ~ "Chems",
  sic %in% c(3570:3579, 3660:3692, 3694:3699, 3810:3829, 7370:7379) ~ "BusEq",
  sic %in% c(4800:4899) ~ "Telcm",
  sic %in% c(4900:4949) ~ "Utils",
  sic %in% c(5000:5999, 7200:7299, 7600:7699) ~ "Shops",
  sic %in% c(2830:2839, 3693, 3840:3859, 8000:8099) ~ "Hlth",
  sic %in% c(6000:6999) ~ "Money",
  TRUE ~ "Other"
)]
# Check which observations are valid -----------------
chars[, valid_data := T]
# Check that stock is eligible for portfolio-ml
chars %>% setorder(id, eom)
lb <- pf_set$lb_hor+1 # Plus 1 to get the last signal of last periods portfolio for portfolio-ML
chars[, eom_lag := shift(eom, n = lb, type="lag"), by = id]
chars[, month_diff := interval(eom_lag, eom) %/% months(1)]
print(paste0("   Valid lookback observation screen excludes ", round(chars[valid_data==T, mean(month_diff != lb | is.na(month_diff))] * 100, 2), "% of the observations"))
chars[, valid_data := (valid_data==T & month_diff==lb & !is.na(month_diff))]
chars[, c("eom_lag", "month_diff") := NULL]
rm(lb)

# Size based screen --------------------------------------
chars |> size_screen_fun(type=settings$screens$size_screen)

# Addition/deletion rule ----------------------------------
chars |> addition_deletion_fun(addition_n=settings$addition_n, deletion_n=settings$deletion_n)

# Show investable universe --------------------------------
chars[valid==T, .N, by = eom] %>% ggplot(aes(eom, N)) + geom_point() + labs(y = "Valid stocks") + geom_hline(yintercept = 0)

# Valid summary
print(paste0("   The valid_data subset has ", round(chars[, mean(valid==T)]*100, 2), "% of the observations and ", round((sum(chars[valid==T]$me) / sum(chars$me))*100, 2), "% of the market cap"))

# Daily returns ----------------------------------
daily <- fread("Data/usa_dsf.csv", colClasses = c("date"="character"), select = c("id", "date", "ret_exc"))
daily <- daily[!is.na(ret_exc) & id<=99999 & id %in% unique(chars[valid==T]$id)]; gc() # Only CRSP observations
daily[, date := date %>% fast_strptime(format="%Y%m%d") %>% as.Date()]; gc()
daily[, eom := date %>% ceiling_date(unit = "month")-1]