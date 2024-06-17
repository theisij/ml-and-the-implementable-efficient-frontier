# How do weigths differ for PF-ML, Market, and Markowitz
d <- as.Date("2020-11-30")
w_ex <- rbind(
  static$w[eom==d, .(id, eom, w, type = "Static-ML*")],  
  pfml$w[eom==d, .(id, eom, w, type = "Portfolio-ML")],
  mkt$w[eom==d, .(id, eom, w, type = "Market")],
  tpf$w[eom==d, .(id, eom, w, type = "Markowitz-ML")]
)
w_ex <- chars[, .(id, eom, dolvol)][w_ex, on = .(id, eom)]

w_ex[, n := .N, by = .(id, eom)]
w_ex <- w_ex[n==max(n)] 

set.seed(settings$seed_no)
sample_ids <- sample(unique(w_ex$id), 100)

# w_ex %>% mutate(w = abs(w)) %>% pivot_wider(names_from = type, values_from = w) %>% select(-id, -n) %>% cor(method = "spearman")

w_ex[, cor(dolvol, abs(w), method = "spearman"), by = type]

w_ex %>%
  group_by(id) %>%
  # mutate(sort_var = w[type=="Market"]) %>%
  filter(id %in% sample_ids) %>%
  filter(type %in% c("Markowitz-ML", "Portfolio-ML")) %>%
  ggplot(aes(reorder(id, dolvol), abs(w))) +
  geom_col() +
  # geom_point() +
  coord_flip() +
  facet_wrap(~type, scales = "free_x") +
  labs(x = "Absolute portfolio weight", y = "Dollar volume rank")

w_ex %>%
  group_by(type) %>%
  mutate(dolvol_rank = frank(dolvol)) %>%
  ggplot(aes(dolvol_rank, abs(w))) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~type, scales = "free_y")



# Portfolio analysis ----------
w_ex <- rbind(
  static$w[, .(id, eom, w, type = "Static-ML*")],  
  pfml$w[, .(id, eom, w, type = "Portfolio-ML")],
  mkt$w[, .(id, eom, w, type = "Market")],
  tpf$w[, .(id, eom, w, type = "Markowitz-ML")]
)
w_ex <- chars[, .(id, eom, dolvol)][w_ex, on = .(id, eom)]

w_ex[, n := .N, by = .(id, eom)]
w_ex <- w_ex[n==max(n)] 

w_ex[, dv_pf := ceiling(ecdf(dolvol)(dolvol)*10), by = .(type, eom)]

w_ex[, .(
  w_abs = mean(abs(w))
), by = .(type, eom, dv_pf)][, .(
  w_abs = mean(w_abs)
), by = .(type, dv_pf)] %>%
  filter(type %in% c("Markowitz-ML", "Portfolio-ML", "Market")) %>%
  ggplot(aes(factor(dv_pf), w_abs)) +
  geom_col(fill = colours_theme[1]) +
  facet_wrap(~type, scales = "free_y", ncol = 3) +
  labs(x = "Dollar volume sorted portfolios (1=low)")


w_ex[, long := if_else(w>=0, "Long positions", "Short positions")]
(output$w_liq <- w_ex[, .(
  w_abs = mean(w)
), by = .(type, eom, dv_pf, long)][, .(
  w_abs = mean(w_abs)
), by = .(type, dv_pf, long)] %>%
  filter(type %in% c("Markowitz-ML", "Portfolio-ML", "Market")) %>%
  ggplot(aes(factor(dv_pf), w_abs, fill=long)) +
  geom_col() +
  facet_wrap(~type, scales = "free_y", ncol = 3) +
  # scale_x_continuous(breaks = c(1, seq(5, 20, 5))) +
  labs(x = "Dollar volume sorted portfolios (1=low)", y = "Average stock weight") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ))
