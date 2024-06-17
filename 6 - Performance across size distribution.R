# Download files -------------------------- 
size_cuts <- c("all", "low80_high100", "low60_high80", "low40_high60", "low20_high40", "low00_high20")
size_path <- "Data/Generated/Portfolios/Size/"
size_folders <- list.files(size_path)
pf_by_size <- size_cuts |> map(function(x) {
  folder <- size_folders[str_detect(size_folders, x)]
  rbind(
    fread(paste0(size_path, folder, "/bms.csv")) |> mutate(eom_ret=as.Date(eom_ret)),
    readRDS(paste0(size_path, folder, "/static-ml.RDS"))$pf,
    readRDS(paste0(size_path, folder, "/multiperiod-ml.RDS"))$pf,
    readRDS(paste0(size_path, folder, "/portfolio-ml.RDS"))$pf
  ) |> mutate(size = x)
}) |> 
  rbindlist() |> 
  mutate(
    size = case_when(
      size == "all" ~ "All",
      size == "low80_high100" ~ "Largest (80-100)",
      size == "low60_high80" ~ "Large (60-80)",
      size == "low40_high60" ~ "Mid (40-60)",
      size == "low20_high40" ~ "Small (20-40)",
      size == "low00_high20" ~ "Smallest (00-20)",
    ) |> factor(levels = c("All", "Largest (80-100)", "Large (60-80)", "Mid (40-60)", "Small (20-40)", "Smallest (00-20)")),
    type = if_else(type=="Rank-Weighted", "Rank-ML", type)
  )

# Performance -----------------------------
pf_by_size[, type := type %>% factor(levels = pf_order)]
pf_by_size %>% setorder(size, type, eom_ret)
pf_by_size[, e_var_adj := (r-mean(r))^2, by=.(size, type)]
pf_by_size[, utility_t := r-tc-0.5*e_var_adj*pf_set$gamma_rel]
pf_summary_size <- pf_by_size[, .(
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
), by = .(size, type)][order(size, type)]

# Plots -----------------------------------
(output$by_size <- pf_summary_size |> 
  mutate(obj = if_else(obj < -1000, -1000, obj)) |> # Helps iron out a bug in geom_col that makes Markowitz look weirds
  ggplot(aes(type, obj)) +
  geom_col(fill=colours_theme[1], position="dodge") +
  geom_hline(yintercept = 0) +
  facet_wrap(~size, ncol=3) +
  coord_cartesian(ylim = c(-0.05, NA)) +
  labs(y = "Realized Utility") + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ))
