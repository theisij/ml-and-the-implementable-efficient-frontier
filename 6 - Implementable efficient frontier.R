# Generate HML/Markowitz for multiple volaility levels -----------------------------------
vol_range <- seq(0, 0.5, 0.01)

# Factor-ML
factor_base <- chars %>% factor_ml_implement(dates = dates_oos, n_pfs = settings$factor_ml$n_pfs, wealth = wealth, gam = pf_set$gamma_rel)
factor_base_vol <- factor_base$pf[, sd(r)*sqrt(12)]
factor_ef <- vol_range %>% map(.progress = T, function(vol_target) {
  scale <- vol_target / factor_base_vol  # Target same vol as m2
  copy(factor_base$w)[, w := w*scale][, w_start:=w_start*scale] %>% pf_ts_fun(data = chars, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(vol_target = vol_target)
}) %>% rbindlist() %>% mutate(type = "Factor-ML")
factor_ss <- factor_ef[, .(
  n = .N, 
  inv = mean(inv), 
  to = mean(turnover), 
  r = mean(r)*12, 
  sd = sd(r)*sqrt(12), 
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12, 
  r_tc = mean((r-tc))*12, 
  sr = mean(r-tc)/sd(r)*sqrt(12), 
  obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12
), by = .(vol_target)]

# Markowitz-ML 
tpf_base <- chars |> tpf_implement(cov_list = barra_cov, wealth = wealth, dates = dates_oos, gam = pf_set$gamma_rel)
tpf_base_vol <- tpf_base$pf[, sd(r)*sqrt(12)]
tpf_ef <- vol_range %>% lapply(function(vol_target) {
  scale <- vol_target / tpf_base_vol  # Target same vol as m2
  copy(tpf_base$w)[, w := w*scale][, w_start:=w_start*scale] %>% pf_ts_fun(data = chars, wealth = wealth, gam = pf_set$gamma_rel) %>% mutate(vol_target = vol_target)
}) %>% rbindlist() %>% mutate(type = "Markowitz-ML")
tpf_ss <- tpf_ef[, .(
  n = .N, 
  inv = mean(inv), 
  to = mean(turnover), 
  r = mean(r)*12, 
  sd = sd(r)*sqrt(12), 
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12, 
  r_tc = mean((r-tc))*12, 
  sr = mean(r-tc)/sd(r)*sqrt(12), 
  obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12
), by = .(vol_target)]

# Mean-variance efficient frontier of risky assets ---------------------------------------------
u_vec <- c(seq(-0.5, 0.5, 0.05), 0.6, 0.75, 1, 2) / 12
wealth_0 <- wealth_func(wealth_end = 0, end = settings$split$test_end, market = market, risk_free = risk_free)
mv_risky_ef <- chars |> mv_risky_fun(cov_list=barra_cov, wealth=wealth_0, dates=dates_oos, gam=pf_set$gamma_rel, u_vec)
mv_ss <- mv_risky_ef[, .(
  n = .N, 
  inv = mean(inv), 
  to = mean(turnover), 
  r = mean(r)*12, 
  sd = sd(r)*sqrt(12), 
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12, 
  r_tc = mean((r-tc))*12, 
  sr = mean(r-tc)/sd(r)*sqrt(12), 
  obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12
), by = .(u)]

# Get IEF portfolios ---------------------------------------------------------------------------
ief_path <- "Data/Generated/Portfolios/IEF/"
ief_pfs <- list.files(path = ief_path) |> map(.progress = T, function(x) {
  w <- x |> str_extract("(?<=WEALTH)[^_]+") |> as.numeric()
  g <- x |> str_extract("(?<=GAMMA)[^_]+") |> as.numeric()
  # Portfolios
  rbind(
    fread(paste0(ief_path, x, "/bms.csv")) |> mutate(eom_ret=as.Date(eom_ret)),
    readRDS(paste0(ief_path, x, "/static-ml.RDS"))$pf,
    readRDS(paste0(ief_path, x, "/portfolio-ml.RDS"))$pf,
    readRDS(paste0(ief_path, x, "/static-ml.RDS"))$hps[eom_ret %in% readRDS(paste0(ief_path, x, "/static-ml.RDS"))$pf$eom_ret & k==1 & g==0 & u==1, .(eom_ret=as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Static-ML"]
  ) |> mutate(wealth_end=w, gamma_rel=g)
}) |> rbindlist()
stopifnot(nrow(ief_pfs[, .N, by = .(wealth_end, gamma_rel, type, eom_ret)][N!=1])==0)# Check for duplicates

ief_ss <- ief_pfs[, .(
  inv = mean(inv), 
  to = mean(turnover), 
  r = mean(r)*12, 
  sd = sd(r)*sqrt(12), 
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12, 
  r_tc = mean((r-tc))*12, 
  sr = mean(r-tc)/sd(r)*sqrt(12), 
  obj = (mean(r)-0.5*var(r)*gamma_rel-mean(tc))*12
), by = .(type, wealth_end, gamma_rel)]

ef_ss <- ief_ss[type=="Portfolio-ML"]

# Build inputs for IEF -----------------------------------------------------------------------
# Summary stats
ef_all_ss <- rbind(
  ief_ss[type %in% c("Portfolio-ML", "Static-ML*", "Static-ML")],
  factor_ss |> mutate(gamma_rel = NA, type = "Factor-ML", wealth_end=pf_set$wealth),
  tpf_ss |> mutate(gamma_rel = NA, type = "Markowitz-ML", wealth_end=pf_set$wealth),
  fill=T
) 







# Indifference curves -------
points <- ef_all_ss[
    (type %in% c("Portfolio-ML") & wealth_end == pf_set$wealth & gamma_rel==pf_set$gamma_rel) | 
    (type %in% c("Static-ML*") & wealth_end == pf_set$wealth & gamma_rel==pf_set$gamma_rel) |
    (type %in% c("Static-ML") & wealth_end == pf_set$wealth & gamma_rel==pf_set$gamma_rel) #|
    # (type %in% c("Factor-ML") & wealth_end == pf_set$wealth & vol_target==0.01)
]

indifference_curves <- 1:nrow(points) %>% lapply(function(i) {
  u_target <- points[i, obj]
  sd_target <- points[i, sd]
  # vol_space <- c(
  #   seq(from = floor(sd_target*100)/100, by = -0.01, length.out = 8),
  #   sd_target,
  #   seq(from = ceiling(sd_target*100)/100, by = 0.01, length.out = 8)
  # ) %>% sort()
  # vol_space <- pmax(vol_space, 0) %>% unique()
  seq(0, 0.4, 0.01) %>% lapply(function(v) {
    data.table(sd = v, r_tc = u_target + pf_set$gamma_rel/2*v^2)
  }) %>% rbindlist() %>% mutate(u = u_target)
}) %>% rbindlist()

# Figure 1A ----------------
tpf_slope <- ef_all_ss[type=="Markowitz-ML"]$sr_gross[2] # All the same (except first which is NAN)
static_raw <- ef_all_ss[(type %in% c("Static-ML") & wealth_end == pf_set$wealth & gamma_rel==pf_set$gamma_rel)]
# Plot settings
ef_y_low <- -0.2
tpf_col <- 10
ef_xmax <- 0.35
plot_txt_size <- 2.5
sta_1l_x <- 0.265
sta_1l_y <- static_raw$r_tc



ef_all_ss <- ef_all_ss |> 
  mutate(type = type %>% factor(levels = c(main_types, "Markowitz-ML (gross)")))

(output$ef_all_wo_ic <-  ef_all_ss %>%
  filter(type != "Static-ML") |>
  filter(wealth_end == pf_set$wealth) %>%
  rbind(expand.grid(type = c("Portfolio-ML", "Static-ML*"), r_tc=0, sd=0), fill=T) %>%
  rbind(data.table(type = "Markowitz-ML (gross)"), fill=T) %>%
  ggplot() +
  annotate("text", x=0.1, y=0.31, label = "Without TC", size=plot_txt_size, hjust=0.5) +
  annotate("segment", x=0.1, xend = 0.12, y=0.3, yend=0.2, arrow = arrow(length = unit(.1,"cm")), size=0.1, alpha=0.75) +
  annotate("segment", x=0.1, xend = 0.1, y=0.3, yend=0.22, arrow = arrow(length = unit(.1,"cm")), size=0.1, alpha=0.75) +
  geom_abline(intercept = 0, slope = tpf_slope, alpha = 1, colour = colours_theme[tpf_col]) +
  geom_line(aes(sd, r_tc, colour = type, group = type)) +
  coord_cartesian(xlim = c(0, ef_xmax), ylim = c(ef_y_low, 0.54), expand = F) +
  geom_point(aes(sd, r_tc, colour = type, shape = factor(gamma_rel), group = type), size=2) +
  geom_path(data = mv_ss[r_tc <= 0.7], aes(sd, r_tc), linetype = "dotted", colour = colours_theme[tpf_col]) + # colours_theme[match("Markowitz-ML", main_types)]
  scale_colour_manual(values = colours_theme[c(match(sort(unique(ef_all_ss$type)), main_types), tpf_col)]) +
  # annotate(geom = "point", y = static_raw$r_tc, x = static_raw$sd, shape = "square", size = 2) +
  # annotate("text", x=static_raw$sd-0.01, y=sta_1l_y, label = "Static-ML (one layer)", size=plot_txt_size, hjust=1, vjust=0.5) +
  # annotate("segment", x=static_raw$sd-0.008, xend = static_raw$sd-0.005, y=sta_1l_y, yend=sta_1l_y, arrow = arrow(length = unit(.1,"cm")), size=0.1, alpha=0.75) +
  annotate(geom = "point", y = static_raw$r_tc, x = static_raw$sd, shape = "square", size = 2) +
  annotate("text", x=static_raw$sd, y=0.05, label = "Static-ML (one layer)", size=plot_txt_size, hjust=0.5, vjust=0.5) +
  annotate("segment", x=static_raw$sd, xend = static_raw$sd, y=0.07, yend=static_raw$r_tc-0.02, arrow = arrow(length = unit(.1,"cm")), size=0.1, alpha=0.75) +
  labs(x = "Volatility", y = "Excess returns (net of trading cost)", shape = "Relative Risk Aversion:") +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size=1, shape=NA)),
    shape = "none" #guide_legend(order = 2, label.hjust = 1)
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.5, 0.97),
    legend.direction="horizontal",
    legend.text = element_text(size=8)
  ) +
  scale_x_continuous(expand = c(0, 0.01)))
output$ef_all <- copy(output$ef_all_wo_ic)
for (u_tgt in points$obj) {
  output$ef_all <- output$ef_all + geom_path(data = indifference_curves[u==u_tgt], aes(sd, r_tc), linetype = "dashed", alpha=0.40)
}
output$ef_all

# Figure 1B --------
comb_data <- ef_ss %>%
  rbind(expand.grid(wealth_end=settings$ef$wealth, sd=0, r_tc=0), fill=T) %>%
  mutate(
    type = "tpf",
    wealth_end = if_else(wealth_end==1, 1, wealth_end)
  ) %>%
  rbind(
    mv_ss %>% mutate(wealth_end=1, type = "mv"), #  %>% mutate(wealth_end=0, type = "mv", r_tc = r_tc+0.04)If we want risky frontier to be tangent for illustrative purposes
    # pf_summary %>% 
    #   filter(type %in% c("Portfolio Sort", "Markowitz")) %>% 
    #   mutate(wealth_end = pf_set$wealth, gamma_rel = pf_set$gamma_rel, r_tc = -Inf, sd = if_else(type == "Markowitz", Inf, sd), type = "bm"),
    fill=T
  ) %>%
  mutate(gamma_rel = gamma_rel %>% factor(levels = settings$ef$gamma_rel)) %>%
  filter(wealth_end %in% c(1, 1e9, 1e10, 1e11))

(output$ef <- comb_data %>%
   ggplot(aes(sd, r_tc, colour = factor(wealth_end), group = factor(wealth_end))) +
   geom_blank() +
   # geom_point(data = comb_data %>% filter(type == "bm"), aes(shape = gamma_rel), size=3) +
   geom_line(data = comb_data %>% filter(type == "tpf")) +
   geom_point(data = comb_data %>% filter(type == "tpf"), aes(shape = gamma_rel), size=2) +
   geom_path(data = comb_data %>% filter(type == "mv"), linetype = "dotted") +
   coord_cartesian(xlim = c(0, ef_xmax), ylim = c(0, 0.75), expand=F) +
   labs(x = "Volatility", y = "Excess returns (net of trading cost)", shape = "Relative Risk Aversion:", 
        colour = "Wealth by 2020:") +
   guides(
     colour = guide_legend(order = 1, override.aes = list(size=1, shape=NA)),
     shape = "none" #guide_legend(order = 2, label.hjust = 1)
   ) +
   theme(
     legend.position = c(0.5, 0.97),
     legend.direction="horizontal",
     legend.title = element_text(size=8),
     legend.text = element_text(size=8)
   ))

