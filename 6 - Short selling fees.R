# Load shorting data
short_fees <- fread("Data/short_fees.csv")
short_fees <- short_fees[!is.na(dcbs) & record_type==1 & market_area=="USA Equity" & !is.na(permno)]

# Expand shorting data
if (FALSE) {
  # Check that shorting fees are somewhat stable over time and DCBS group
  short_fees[!is.na(indicative_fee), .(mean=mean(indicative_fee), median=median(indicative_fee)), by = .(date, dcbs)] %>%
    pivot_longer(-c(date, dcbs)) %>%
    ggplot(aes(date, value, colour=name)) + 
    geom_point(size=0.5, alpha = 0.5) +
    # geom_line() +
    facet_wrap(~factor(dcbs), scales = "free_y") # Declining trend is natural as Markit adds more stocks
}
# For stocks in the data --------
short_fees[, sfee := if_else(!is.na(indicative_fee), indicative_fee, mean(indicative_fee, na.rm=T)), by = dcbs]

# Take the last observation in a month for each stock
short_fees[, eom := ceiling_date(date, unit="m")-1]
short_fees[, max_date := max(date), by = .(permno, eom)]
short_fees <- short_fees[date==max_date, .(permno, eom, sfee, dcbs)]

# Remove duplicates  # Very rare: short_fees[, .N, by = .(permno, eom)][, table(N)]
short_fees[, n := .N, by = .(permno, eom)]
short_fees <- short_fees[n == 1][, n := NULL]

# Compute shorting fees for stocks not in the sample ---------------------
# Analysis
if (FALSE) {
  short_fees2 <- short_fees[, .(id=permno, eom, sfee, dcbs)][chars[, .(id, eom, market_equity, rvol_252d, dolvol_126d)], on = .(id, eom)]
  # How many are non-missing? Very few
  short_fees2[, .(non_miss=mean(!is.na(sfee))), by = eom][non_miss != 0] %>% ggplot(aes(eom, non_miss)) + geom_point()
  # What is the typical dcbs group? 96% of the sample are in the "easiest to borrow" group
  short_fees2[!is.na(dcbs), table(dcbs)/.N] 
  # Surprisingly difficult to predict 
  felm(sfee~market_equity | eom | 0 | eom+id, data = short_fees2) %>% summary()
  felm(sfee~market_equity+rvol_252d | eom | 0 | eom+id, data = short_fees2) %>% summary()
  felm(sfee~market_equity+rvol_252d+dolvol_126d | eom | 0 | eom+id, data = short_fees2) %>% summary()
  # Maybe it's caused by outliers? It helps, but not a lot
  short_fees2[!is.na(sfee) & eom==max(eom)] %>% ggplot(aes(market_equity, sfee)) + geom_point()
  felm(sfee~market_equity | eom | 0 | eom+id, data = short_fees2[sfee <= quantile(short_fees2$sfee, probs=0.99, na.rm=T)]) %>% summary()
  felm(sfee~market_equity+rvol_252d | eom | 0 | eom+id, data = short_fees2[sfee <= quantile(short_fees2$sfee, probs=0.99, na.rm=T)]) %>% summary()
  felm(sfee~market_equity+rvol_252d+dolvol_126d | eom | 0 | eom+id, data = short_fees2[sfee <= quantile(short_fees2$sfee, probs=0.99, na.rm=T)]) %>% summary()
  # Maybe impute by median borrow fee for stocks in sample?
  short_fees2[!is.na(sfee), .(n = .N, mean=mean(sfee), median=median(sfee))]
  short_fees2[!is.na(sfee), .(n = .N, mean=mean(sfee), median=median(sfee)), by = dcbs][, n := round(n / sum(n), 2)][order(dcbs)]
}
# Impute outside of sample using median
short_fees <- short_fees[, .(id=permno, eom, sfee)][chars[, .(id, eom)], on = .(id, eom)]
short_fees[, sfee := if_else(!is.na(sfee), sfee, median(sfee, na.rm=T))]

# Incorporate shorting cost of each method -------------------------------------
list_short <- list(
  "Multiperiod-ML*" = mp$w, 
  "Portfolio-ML" = pfml$w, 
  "Static-ML*" = static$w, 
  "Markowitz-ML" = tpf$w, 
  "Factor-ML" = factor_ml$w, 
  "1/N" = ew$w, 
  "Market" = mkt$w, 
  "Rank-ML" = rw$w, 
  "Minimum Variance" = mv$w
)

shorting_costs_ts <- names(list_short) %>% lapply(function(nm) {
  w <- list_short[[nm]]
  # Pay 100% of fee if short get 70% of fee if long [from p.10 in "anomalies and their short-sale cost"] 
  short_cost <- short_fees[w, on = .(id, eom)][w<0, .(short_cost = sum(sfee*(-w))), by = eom] #[order(shorting_cost)][, mean(shorting_cost)]
  long_rev <- short_fees[w, on = .(id, eom)][w>0, .(long_rev = sum(sfee*w)), by = eom] #[order(shorting_cost)][, mean(shorting_cost)]
  sub <- w[, .(long = sum(w[w>0]), short = sum(w[w<0]), net_lev = sum(w)), by = eom][, type := nm]
  sub <- short_cost[sub, on = "eom"]
  long_rev[sub, on = .(eom)]
}) %>% rbindlist()

# Shorting costs
shorting_costs <- shorting_costs_ts[, lapply(.SD, mean), .SDcols = c("long_rev", "short_cost", "long", "short", "net_lev"), by = type]
shorting_costs[short==0, short_cost := 0]
shorting_costs[, net_short_cost := short_cost-long_rev]

# Output: Shorting costs are almost completely unimportant for our strategies [but this is ofcourse conditional on our ability to borroe]
(test_tbl <- pf_summary[shorting_costs, on = .(type)][, .(type, long, short, short_cost, long_rev, net_short_cost, 
                                                          r_tc, 
                                                          r_tc_short = r_tc-short_cost,
                                                          r_tc_short_net = r_tc-net_short_cost,
                                                          sr, 
                                                          sr_short = (r_tc-short_cost)/sd,
                                                          sr_short_net = (r_tc-net_short_cost)/sd)])
test_tbl[, .(type, sr, sr_short, sr_short_net)]

# Alternative 1 
test_tbl %>% 
  mutate(type = type %>% factor(levels = pf_order_new)) %>%
  arrange(type) %>%
  select(-c("r_tc", "r_tc_short", "r_tc_short_net")) %>%
  xtable(digits = c(0, 0, rep(2, 2), rep(4, 3), rep(2, 3))) %>%
  print(include.rownames=FALSE)

# Alternative 2
test_tbl %>% 
  mutate(type = type %>% factor(levels = pf_order_new)) %>%
  arrange(type) %>%
  select(-c("sr", "sr_short", "sr_short_net")) %>%
  xtable(digits = c(0, 0, rep(2, 2), rep(4, 3), rep(2, 3))) %>%
  print(include.rownames=FALSE)

# Figure -------------
(output$shorting <- pf_summary[shorting_costs, on = .(type)] |> 
  filter(type %in% c("Portfolio-ML", "Multiperiod-ML*", "Static-ML*")) |>
  # select(type, shorting, r_tc, sd, obj, short_cost, long_rev) |> 
  mutate(
    type = type %>% factor(levels = pf_order),
    obj_gross_short = obj - short_cost,
    obj_net_short = obj - short_cost + long_rev
  ) |> 
  select(type, obj, obj_gross_short, obj_net_short) |>
  pivot_longer(-type) |> 
  mutate(
    name = case_when(
      name == "obj" ~ "Utility",
      name == "obj_gross_short" ~ "Utility - short costs",
      name == "obj_net_short" ~ "Utility - short costs + long revenue"
    )
  ) |>
  ggplot(aes(type, value, fill=name)) +
  geom_col(position = position_dodge(width = 0.93)) +
  geom_text(aes(label = formatC(value, format="f", digits=3)), position = position_dodge(width = 0.93), vjust = -0.2) +
  coord_cartesian(ylim = c(0, 0.1)) +
  labs(y = "Utility") +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ))
  
# Plot: Utility: without shorting costs, with only gross shorting costs, with new shorting costs
  
  
