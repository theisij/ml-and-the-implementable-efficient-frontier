# FI in base case -----------------------
fi_path <- "Data/Generated/Portfolios/FI/"
fi_folder <- paste0(fi_path, list.files(fi_path))

tpf_cf_base <- fread(paste0(fi_folder, "/tpf_cf_base.csv"))
pfml_cf_base <- fread(paste0(fi_folder, "/pfml_cf_base.csv"))
ret_cf <- fread(paste0(fi_folder, "/ret_cf.csv"))
pfml_cf_ief <- fread(paste0(fi_folder, "/pfml_cf_ief.csv"))

pfml_cf_ss <- pfml_cf_base %>% 
  group_by(type, cluster) %>% 
  summarise(cf_obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12) %>%
  ungroup() %>%
  mutate(fi = cf_obj[cluster=="bm"] - cf_obj) |> 
  select(type, cluster, fi)


tpf_cf_ss <- tpf_cf_base %>%
  group_by(type, cluster) %>% 
  summarise(cf_obj = (mean(r)-0.5*var(r)*pf_set$gamma_rel-mean(tc))*12) %>%
  ungroup() %>%
  mutate(fi = cf_obj[cluster=="bm"]-cf_obj) |> 
  select(type, cluster, fi)

(output$feature_importance <- rbind(
  pfml_cf_ss %>% mutate(wealth = 1e10),
  tpf_cf_ss %>% mutate(wealth = 0)#,
  # shap_er1 %>% mutate(type = "Expected 1m Return") %>% select(type, cluster, fi) %>% mutate(wealth = 0)
) %>%
    filter(cluster != "bm") |> 
    group_by(type) %>%
    # mutate(fi = fi / max(fi)) %>%
    group_by(cluster) %>%
    mutate(
      sort_var = sum(fi*(type == "Portfolio-ML")),
      # sort_var = sum(fi*(type == "Expected 1m Return")),
      cluster = cluster %>% str_replace_all("_", " ") %>% str_replace("short term", "short-term") %>% str_to_title(),
      type = type %>% factor(levels = c("Portfolio-ML", "Multiperiod-ML*", "Markowitz-ML", "Expected 1m Return"))
    ) %>%
    filter(type != "Expected 1m Return") %>%
    ggplot(aes(reorder(cluster, sort_var), fi, fill = type)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c(colours_theme[1], colours_theme[2], colours_theme[5])) + 
    coord_flip() +
    facet_wrap(~type, nrow = 1, scales = "free_x") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "black")
    ) +
    labs(y = "Drop in realized utility from permuting theme features"))


# FI in IEF -------------------------
# Summary stats
ef_cf_ss <- pfml_cf_ief[, .(
  obj = (mean(r)-0.5*var(r)*gamma_rel-mean(tc))*12,
  r_tc = mean(r-tc)*12,
  sd = sd(r)*sqrt(12)
), by = .(gamma_rel, cluster)][, sr := r_tc / sd] %>% rename("shuffled"=cluster)
# Add benchmark
ef_cf_ss <- ef_cf_ss %>% rbind(ef_ss[wealth_end==pf_set$wealth, .(gamma_rel, shuffled = "none", obj, r_tc, sd, sr)])

# With trading costs
sub <- c("Quality", "Value", "Short-Term Reversal", "None")
(output$cf_ef_tc <- ef_cf_ss %>%
    rbind(expand.grid(shuffled = unique(ef_cf_ss$shuffled), sd=0, r_tc=0), fill=T) %>%
    mutate(
      shuffled = shuffled %>% str_replace_all("_", " ") %>% str_replace_all("short term", "short-term") %>% str_to_title(),
      shuffled = shuffled %>% factor(levels = c("None", "Quality", "Value", "Short-Term Reversal", "Momentum"))
    ) %>%
    filter(shuffled %in% sub) %>%
    ggplot(aes(sd, r_tc, colour = factor(shuffled), group = factor(shuffled), shape = factor(gamma_rel), linetype=factor(shuffled))) +
    geom_line() +
    geom_point(size=2) +
    # coord_cartesian(xlim = c(0, 0.6), ylim = c(0, 0.5)) +
    coord_cartesian(xlim = c(0, 0.25), ylim = c(0, 0.28), expand=F) +
    # scale_x_continuous(breaks = seq(0, 0.3, 0.1)) +
    labs(x = "Volatility", y = "Excess returns (net of trading cost)", shape = "Relative Risk Aversion:", 
         colour = "Theme permuted:", linetype = "Theme permuted:") +
    guides(
      colour = guide_legend(order = 1, override.aes = list(size=1, shape=NA)),
      linetype = guide_legend(order = 1),
      shape = "none" #guide_legend(order = 2, label.hjust = 1)
    ) +
    theme(
      legend.position = c(0.5, 0.97),
      legend.direction="horizontal",
      legend.justification = "center"
    ))

# Counterfactual EF without TC --------------------------
tpf_cf_ss <- tpf_cf_base[, .(
  sr = mean(r)/sd(r)*sqrt(12)
), by = cluster][cluster=="bm", cluster := "none"]

x_values <- data.table(sd = seq(0, 0.35, 0.01))

(output$cf_ef_markowitz <- tidyr::crossing(tpf_cf_ss, x_values) %>%
    mutate(
      ret = sd*sr,
      shuffled = cluster %>% str_replace_all("_", " ") %>% str_replace_all("short term", "short-term") %>% str_to_title(),
      shuffled = shuffled %>% factor(levels = c("None", "Quality", "Value", "Short-Term Reversal", "Momentum"))
    ) %>%
    filter(shuffled %in% sub) %>%
    ggplot(aes(sd, ret, colour = factor(shuffled), group = factor(shuffled), linetype = factor(shuffled))) +
    geom_line() +
    # coord_cartesian(xlim = c(0.15, 0.3), ylim = c(0.25, 0.5)) +
    coord_cartesian(xlim = c(0, 0.25), ylim = c(0, 0.55), expand=F) +
    # coord_cartesian(xlim = c(0, 0.165), ylim = c(0, 0.35), expand=F) +
    labs(x = "Volatility", y = "Excess returns", colour = "Theme permuted:", linetype = "Theme permuted:") +
    guides(
      colour = guide_legend(order = 1, override.aes = list(size=1)),
      linetype = guide_legend(order = 1)
    ) +
    theme(
      legend.position = c(0.5, 0.97),
      legend.direction="horizontal"
    )) 

# Feature importance for return predictions models --------------------------------
ret_cf_ss <- ret_cf[, .(mse = mean(mse)), by = .(h, cluster)]
bm <- ret_cf_ss[cluster == "bm", .(h, bm=mse)]
ret_cf_ss <- bm[ret_cf_ss[cluster != "bm"], on = "h"]      
ret_cf_ss[, fi := mse-bm]

if (TRUE) {
  (output$fi_returns <- ret_cf_ss %>%
     mutate(
       cluster = cluster %>% str_replace_all("_", " "),
       cluster = cluster %>% str_replace_all("short term", "short-term"),
       cluster = cluster %>% str_to_title()
     ) %>%
     group_by(h) %>%
     mutate(fi = fi / max(fi)) %>%
     group_by(cluster) %>%
     mutate(sort_var = fi[h==1]) %>%
     ggplot(aes(reorder(cluster, sort_var), fi, fill=factor(h))) +
     geom_col(position = "dodge") +
     # Scale_y_continuous with percentages
     scale_y_continuous(labels = scales::label_percent(), breaks = seq(-0.2, 1, 0.2)) +
     geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
     labs(y = "Drop in MSE from permuting theme features (% of max)", fill = "Horizon") + # Otherwise difficult to visualize due to small x-axis values
     theme(
       axis.title.x = element_blank()
     ))
} else {
  (output$fi_returns <- ret_cf_ss %>%
     mutate(
       title = paste0("Horizon: ", h, " month") |> factor(levels = paste0("Horizon: ", 1:12, " month")),
       cluster = cluster %>% str_replace_all("_", " "),
       cluster = cluster %>% str_replace_all("short term", "short-term"),
       cluster = cluster %>% str_to_title()
     ) %>%
     group_by(h) %>%
     mutate(fi = fi / max(fi) * 100) %>%
     group_by(cluster) %>%
     mutate(sort_var = fi[h==1]) %>%
     ggplot(aes(reorder(cluster, sort_var), fi)) +
     geom_col(fill = colours_theme[1]) +
     coord_flip() +
     facet_wrap(~title, scales = "free_x") +
     labs(y = "Drop in MSE from permuting theme features (% of max)") + # Otherwise difficult to visualize due to small x-axis values
     theme(
       axis.title.y = element_blank()
     ))
}

# Wierdly, ret_1_0 strongly predicts t+12? But note that it's with the opposite sign of t+1 (I know! It's the seasonality effect)
melt(chars[!is.na(pred_ld1), c("id", "eom", paste0("pred_ld", 1:12), "ret_1_0", "ret_12_1", "be_me", "gp_at")], id.vars = c("id", "eom", paste0("pred_ld", 1:12)))[, lapply(.SD, function(x) cor(value, x)), .SDcols = paste0("pred_ld", 1:12), by = .(variable, eom)][, lapply(.SD, mean), by = variable] |> 
  select(-eom) |> 
  pivot_longer(-variable, names_to = "h", values_to = "cor") |>
  mutate(h = h |> str_remove("pred_ld") |> as.numeric()) |>
  ggplot(aes(h, cor, colour = variable)) +
  geom_hline(yintercept = 0) + 
  geom_line() +
  geom_point() +
  theme(
    legend.position = "top"
  )
