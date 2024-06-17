# Need to run everything from main to right before bm_pfs in 5 - base case
# Load base case portfolios -----------
base_path <- "Data/Generated/Portfolios/Base/"
base_folder <- list.files(base_path)
mp <- readRDS(paste0(base_path, base_folder, "/multiperiod-ml.RDS"))
pfml <- readRDS(paste0(base_path, base_folder, "/portfolio-ml.RDS"))
static <- readRDS(paste0(base_path, base_folder, "/static-ml.RDS"))
bm_pfs <- fread(paste0(base_path, base_folder, "/bms.csv")) |> 
  mutate(
    eom_ret = as.Date(eom_ret),
    type = if_else(type == "Rank-Weighted", "Rank-ML", type) # Old naming convention
  )

# Final Portfolios ---------------------
pfs <- rbind(
  mp$pf,
  pfml$pf,
  static$pf, 
  bm_pfs,
  mp$hps[eom_ret %in% mp$pf$eom_ret & k==1 & g==0 & u==1, .(eom_ret=as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Multiperiod-ML"],
  static$hps[eom_ret %in% static$pf$eom_ret & k==1 & g==0 & u==1, .(eom_ret=as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Static-ML"]
)
pfs[, type := type %>% factor(levels = pf_order)]
pfs %>% setorder(type, eom_ret)
pfs[, e_var_adj := (r-mean(r))^2, by=type]
pfs[, utility_t := r-tc-0.5*e_var_adj*pf_set$gamma_rel]

# Portfolio summary stats --------------
pf_summary <- pfs[, .(
  n = .N,
  inv = mean(inv),
  shorting = mean(shorting),
  turnover_notional = mean(turnover),
  r = mean(r)*12,
  sd = sd(r)*sqrt(12),
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12,
  r_tc = mean((r-tc))*12,
  sr = mean(r-tc)/sd(r)*sqrt(12),
  obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12#,
  # obj_exp = (mean(e_r)-0.5*mean(e_var)*pf_set$gamma_rel-mean(tc))*12
), by = .(type)][order(type)]
# Remove not essential types
pfs <- pfs[type %in% main_types][, type := type %>% factor(levels = main_types)]

# Performance Time-Series -----------
types <- unique(pfs$type)
pfs[, cumret := cumsum(r), by=.(type)]
pfs[, cumret_tc := cumsum(r-tc), by=.(type)]
pfs[, cumret_tc_risk := cumsum(utility_t), by=.(type)]
ts_data <- pfs[type %in% main_types, .(type, eom_ret, cumret, cumret_tc, cumret_tc_risk)] %>%
  pivot_longer(-c(type, eom_ret)) %>%
  bind_rows(tibble(
    eom_ret = ceiling_date(min(pfs$eom_ret), unit = "month")-1,
    type = rep(types, 3),
    value = rep(0, 3*length(types)),
    name = c(rep("cumret", length(types)), rep("cumret_tc", length(types)), rep("cumret_tc_risk", length(types)))
  )) %>%
  mutate(
    name_pretty = case_when(
      name == "cumret" ~ "Gross return",
      name == "cumret_tc" ~ "Return net of TC",
      name == "cumret_tc_risk" ~ "Return net of TC and Risk"
    ) %>% factor(levels = c("Gross return", "Return net of TC", "Return net of TC and Risk"))
  ) %>%
  setDT()

ts_plots <- unique(ts_data$name) %>% lapply(function(x) {
  p <- ts_data[name == x] %>%
    ggplot(aes(eom_ret, value, colour=type, linetype=type)) +  
    geom_line() +
    coord_cartesian(ylim=c(0, NA)) +
    facet_wrap(~name_pretty, scales = "free") +
    labs(y = "Cummulative performance", colour = "Method:", linetype="Method:") +
    scale_x_date(date_labels = "%Y", breaks = seq.Date(as.Date("1980-12-31"), as.Date("2020-12-31"), by = "20 years")) +
    theme(
      axis.title.x = element_blank(),
      strip.background = element_rect(fill = "white", color = "black"),
      text = element_text(size=11)
    )
  if (x == "cumret") {
    y_range <- ts_data[name == x & type != "Markowitz-ML" & name=="cumret", .(max = max(value), min = min(value))]
    p <- p + coord_cartesian(ylim=c(y_range$min, y_range$max)) 
  }
  p
})
# ts_legend <- cowplot::get_legend(
#   ts_plots[[1]] + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "top", legend.title = element_blank())
# )
ts_legend <- cowplot::get_plot_component(
  plot = ts_plots[[1]] + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "top", legend.title = element_blank(), legend.justification = "center"), 
  'guide-box-top', return_all = TRUE)
ts_plot <- cowplot::plot_grid(
  ts_plots[[1]] + theme(legend.position = "none"),
  ts_plots[[2]] + theme(axis.title.y = element_blank(), legend.position = "none"),
  ts_plots[[3]] + theme(axis.title.y = element_blank(), legend.position = "none"),
  nrow = 1, rel_widths = c(1.07, 1, 1)
)
(output$ts <- cowplot::plot_grid(ts_legend, ts_plot, ncol=1, rel_heights = c(.1, 1)))

# Test probability of outperformance ---------------------------------------------------
pfs_wide <- pfs[, .(type, eom_ret, utility_t)] %>% dcast(eom_ret~type, value.var = "utility_t")

prob_outperformance <- main_types %>% lapply(function(t) {
  x <- pfs_wide %>% 
    melt(id.vars = c("eom_ret", t), variable.name = "alt")
  x[, diff := get(t)-value, by = alt]
  x[, .(
    prob_main_op = 1-pnorm(q = 0, mean=mean(diff), sd = sd(diff)/sqrt(length(diff)))
  ), by = alt] %>% mutate(main = t)
}) %>% rbindlist()

# Portfolio statistics over time --------------------
# Compute expected portfolio risk
ws <- rbind(
  pfml$w |> mutate(type = "Portfolio-ML"),
  tpf$w |> mutate(type = "Markowitz-ML"),
  mp$w |> mutate(type = "Multiperiod-ML*"),
  static$w |> mutate(type = "Static-ML*") |> select(-pred_ld1),
  factor_ml$w |> mutate(type = "Factor-ML")
) |> mutate(type = type %>% factor(levels = pf_order))
pf_vars <- ws |> expected_risk_fun(dates=dates_oos, cov_list=barra_cov)
# Compute portfolios statistics
(output$comp_stats <- pf_vars[pfs[, .(type, "eom"=eom_ret+1-months(1)-1, inv, turnover)], on = .(type, eom)] %>% # , e_sd
    filter(type %in% main_types) |> 
    mutate(e_sd = sqrt(pf_var*252)) |> 
    select(-pf_var) |> 
    pivot_longer(-c(type, eom)) %>%
    mutate(
      name = case_when(
        name == "e_sd" ~ "Ex-ante Volatility",
        name == "turnover" ~ "Turnover",
        name == "inv" ~ "Leverage"
      ) 
    ) %>%
    ggplot(aes(eom, value, colour=type, linetype=type)) +
    geom_line() +
    scale_y_log10() +
    labs(y = "Value") +
    facet_wrap(~name, scales="free_y", ncol=1) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ))

# Correlation ----------------------------------
pf_cors <- pfs[, .(eom_ret, r, type)] %>%
  pivot_wider(names_from = type, values_from = r) %>%
  select(-eom_ret) %>%
  cor(method = "pearson") 

pf_cors %>%
  corrplot::corrplot(method = "number", type = "lower")

# Apple vs. Xerox ----------------------------------------------------
liquid <- 22111 # 14593
# liquid <- c(93436, 14593, 91103, 88352, 19561, 10107)
illiquid <- 27983 # Xerox (illiquid stock with all observations: data_tc[valid==T & year(eom)>=2015][, n := .N, by = id][n==max(n)][, .(id, eom, n, me, dolvol)][eom==max(eom)][order(dolvol)])
# illiquid <- 26825 # Kellog (smallest stock with all observations: data_tc[valid==T & size_grp=="mega"][, n := .N, by = id][n == max(n)][, .(id, eom, n, me)][eom==max(eom)][order(me)])
position <- rbind(
  mp$w[id %in% c(liquid, illiquid)][, type := "Multiperiod-ML*"],
  pfml$w[id %in% c(liquid, illiquid)][, type := "Portfolio-ML"],
  static$w[id %in% c(liquid, illiquid)][, type := "Static-ML*"],
  tpf$w[id %in% c(liquid, illiquid)][, type := "Markowitz-ML"],
  factor_ml$w[id %in% c(liquid, illiquid)][, type := "Factor-ML"],
  mkt$w[id %in% c(liquid, illiquid)][, type := "Market"], 
  fill=T
) %>% mutate(
  stock_type = case_when(
    id == 14593 ~ "Apple (liquid)",
    id == 27983 ~ "Xerox (illiquid)",
    id == 93436 ~ "Tesla",
    id == 91103 ~ "Visa",
    id == 19561 ~ "Boing",
    id == 10107 ~ "Microsoft",
    id == 22111 ~ "Johnson and Johnson (liquid)",
    id == 55976 ~ "Walmart (liquid)",
    TRUE ~ as.character(id)
  )
)
position <- position[year(eom)>=2015]
position <- pfs[, .(type, "eom"=eom_ret %>% ceiling_date(unit="m")-1, inv)][position, on = .(type, eom)]
position[, w_z := (w-mean(w))/sd(w), by = .(type, id)]

(output$example <- position %>%
  mutate(type = type %>% factor(levels = pf_order)) %>%
  ggplot(aes(eom, w, colour=stock_type, linetype = stock_type)) +
  # geom_point(alpha=0.2) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~type, scales = "free_y") +
  labs(y = "Portfolio weight") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.justification = "center",
    strip.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_blank()
  ))

if (FALSE) {
  position %>%
    mutate(tc_type = (type %in% c("Markowitz-ML", "Portfolio Sort"))) %>%
    filter(type != "Portfolio Sort") %>%
    ggplot(aes(eom, w_z, colour=type)) + # , alpha = tc_type
    # geom_point(alpha=0.2) +
    geom_line(alpha=0.6) +
    # scale_alpha_discrete(range = c(0.6, 0.35)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~stock_type)
}

# Optimal Hyper-parameters ----------------------------
model_1 <- readRDS(paste0(get_from_path_model, "/model_1.RDS"))
model_6 <- readRDS(paste0(get_from_path_model, "/model_6.RDS"))
model_12 <- readRDS(paste0(get_from_path_model, "/model_12.RDS"))
(output$er_tuning <- rbind(
  model_1 %>% lapply(function(x) {
    x$pred[, .(eom_ret = eom+1+months(1)-1)] %>% cbind(x$opt_hps[, .(lambda, p, g)]) %>% unique()
  }) %>% rbindlist() %>% filter(!is.na(eom_ret)) %>% mutate(horizon = "Return t+1"),
  model_6 %>% lapply(function(x) {
    x$pred[, .(eom_ret = eom+1+months(1)-1)] %>% cbind(x$opt_hps[, .(lambda, p, g)]) %>% unique()
  }) %>% rbindlist() %>% filter(!is.na(eom_ret)) %>% mutate(horizon = "Return t+6"),
  model_12 %>% lapply(function(x) {
    x$pred[, .(eom_ret = eom+1+months(1)-1)] %>% cbind(x$opt_hps[, .(lambda, p, g)]) %>% unique()
  }) %>% rbindlist() %>% filter(!is.na(eom_ret)) %>% mutate(horizon = "Return t+12")
) %>%
  mutate(lambda = log(lambda)) |> 
  rename("eta"=g, "log(lambda)"=lambda) %>%
  pivot_longer(-c(horizon, eom_ret)) %>%
  mutate(
    comb_name = paste0("'", horizon, ":'~", name) %>% factor(levels = c(
      paste0("'Return t+1:'~", c("log(lambda)", "p", "eta")), 
      paste0("'Return t+6:'~", c("log(lambda)", "p", "eta")), 
      paste0("'Return t+12:'~", c("log(lambda)", "p", "eta")))
    )
  ) %>%
  ggplot(aes(eom_ret, value)) +
  geom_point(alpha=0.75, colour = colours_theme[10]) +
  facet_wrap(~comb_name, scales = "free_y", labeller = label_parsed) +
  labs(y = "Optimal hyper-parameter") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size=9),
    strip.background = element_rect(fill = "white", color = "black"),
    axis.text = element_text(size=8)
  ))

(output$portfolio_tuning <- rbind(
  mp$hps[rank == 1 & year(eom_ret)>=1981 & month(eom_ret)==12, .(eom_ret, k, g, u)][, type := "Multiperiod-ML*"],
  static$hps[rank == 1 & year(eom_ret) >= 1981  & month(eom_ret)==12, .(eom_ret, k, g, u)][, type := "Static-ML*"]
) %>% 
  rename("v"=g) %>%
  pivot_longer(-c(type, eom_ret)) %>%
  rbind(pfml$best_hps[eom_ret>=1981, .(type = "Portfolio-ML", eom_ret, "log(lambda)"=log(l), p, "eta"=g)] %>% pivot_longer(-c(type, eom_ret))) %>%
  mutate(
    comb_name = paste0("'", type, ":'~", name) %>% factor(levels = c(paste0("'Portfolio-ML:'~", c("log(lambda)", "p", "eta")), 
                                                                       paste0("'Multiperiod-ML*:'~", c("k", "u", "v")), 
                                                                       paste0("'Static-ML*:'~", c("k", "u", "v"))))
  ) %>%
  ggplot(aes(eom_ret, value, colour = type)) +
  geom_point(alpha=0.75) +
    facet_wrap(~comb_name, scales = "free_y", labeller = label_parsed) +
  labs(y = "Optimal hyper-parameter") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    text = element_text(size=9),
    strip.background = element_rect(fill = "white", color = "black"),
    axis.text = element_text(size=8)
  ))

# AR1 plot -------------------------------------------
ar1 <- chars[order(id, eom)]
ar1[, lag_ok := (eom - lag(eom)) %in% c(28:31), by = id]

ar1_ss <- features %>% map(.progress = T, function(x) {
  ar1[, var := get(x)]
  ar1[, var_l1 := lag(var), by = id]
  sub <- ar1[!is.na(var) & !is.na(var_l1) & lag_ok == T & var != 0.5 & var_l1 != 0.5 & valid==T][, n := .N, by = id][n >= 12*5] # 0.5 indicates missing
  sub[, .(
    n = .N,
    ar1 = cor(var, var_l1)
  ), by = id][is.na(ar1), ar1 := 1][, .(
    char = x,
    ar1 = mean(ar1)
  )]
}) %>%
  rbindlist()
temp_order <- cluster_labels[, .("char"=characteristic, cluster)][ar1_ss, on = "char"][, mean(ar1), by = cluster][order(V1)]$cluster %>% 
  str_replace_all("_", " ") %>% str_replace("short term", "short-term") %>% str_to_title()
(output$ar1 <- cluster_labels[, .("char"=characteristic, cluster)][ar1_ss, on = "char"] %>%
  group_by(cluster) %>%
  mutate(sort_var = mean(ar1)+ar1/100000) %>%
  mutate(
    pretty_name = cluster %>% str_replace_all("_", " ") %>% str_replace("short term", "short-term") %>% str_to_title() %>%
      factor(levels = temp_order)
  ) %>%
  ggplot(aes(reorder(char, sort_var), ar1, fill=pretty_name)) +
  geom_col() +
  coord_flip() +
  labs(y = "Average Monthly Autocorrelation", fill = "Theme") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    axis.title.y = element_blank()
  ))

# Features with sufficient coverage ------------------
if (FALSE) {
  features_all <- c(features_m, feat_excl)
  
  ids <- c("source_crsp", "common", "obs_main", "primary_sec", "exch_main", "id", "eom", "sic", "ff49", "size_grp", "me", "rvol_21d", "dolvol_126d")
  data <- fread(paste0("../Data/usa.csv"), 
                select = unique(c(ids, features_all)), #, "excntry" 
                colClasses = c("eom" = "character", "sic"="character"))
  data[, eom := eom %>% lubridate::fast_strptime(format = "%Y%m%d") %>% as.Date()]
  data[, dolvol := dolvol_126d] 
  data[, rvol_m := rvol_252d * sqrt(21)]
  data <- data[source_crsp == 1 & common==1 & obs_main==1 & primary_sec==1 & exch_main==1][, c("source_crsp", "common", "obs_main", "primary_sec", "exch_main") := NULL]
  # Screen Data -----------------
  # Start Date Screens
  print(paste0("   Start date screen excludes ", round(mean(data$eom < settings$screens$start) * 100, 2), "% of the observations"))
  data <- data[eom >= settings$screens$start]
  # Monitor screen impact
  n_start <- nrow(data)
  me_start <- sum(data$me, na.rm = T)
  # Require me, dolvol and rvol
  print(paste0("   Non-missing me excludes ", round(mean(is.na(data$me)) * 100, 2), "% of the observations"))
  data <- data[!is.na(me)]
  if (settings$screens$require_rvol) {
    print(paste0("   Non-missing rvol_252d excludes ", round(mean(is.na(data$rvol_m)) * 100, 2), "% of the observations"))
    data <- data[!is.na(rvol_m)]
  }
  if (settings$screens$require_dolvol) {
    print(paste0("   Non-missing dolvol_126d excludes ", round(mean(is.na(data$dolvol)) * 100, 2), "% of the observations"))
    data <- data[!is.na(dolvol)]
  }
  # Size Screen 
  print(paste0("   Size screen excludes ", round(mean(!(data$size_grp %in% settings$screens$size_grps)) * 100, 2), "% of the observations"))
  data <- data[size_grp %in% settings$screens$size_grps]
  # Feature Screens
  feat_available <- data %>% select(all_of(features_all)) %>% apply(1, function(x) sum(!is.na(x)))
  min_feat <- floor(length(features_all)*settings$screens$feat_pct)
  print(paste0("   At least ", settings$screens$feat_pct*100, "% of feature excludes ", round(mean(feat_available < min_feat)*100, 2), "% of the observations"))
  data <- data[feat_available >= min_feat]
  # Summary
  print(paste0("   In total, the final dataset has ", round( (nrow(data) / n_start)*100, 2), "% of the observations and ", round((sum(data$me) / me_start)*100, 2), "% of the market cap in the post ", settings$screens$start, " data"))
  
  # Check coverage by eom [Seems like we could include seas_16_20na and seas_16_20an]
  coverage <- data[, lapply(.SD, function(x) mean(!is.na(x))), by = eom, .SDcols=features_all]
  coverage[eom==min(eom)] %>%
    pivot_longer(-eom) %>%
    arrange(value)
}
