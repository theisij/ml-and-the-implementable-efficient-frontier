# Tables -------------------
# Portfolio summary [Add "\\-1.2em after Markowitz-ML and remove TC, R-TC, SR_new and Utility from Markowtiz-ML]
if (FALSE) {
  pf_summary %>%
    filter(type %in% c("Portfolio-ML", "Multiperiod-ML", "Static-ML", "Factor-ML", "Markowitz-ML",
                       "Multiperiod-ML*", "Static-ML*")) %>%
    rbind(
      data.table(type="One tuning layer"),
      data.table(type="Two tuning layers"), fill=T
    ) %>%
    mutate(
      # tc = if_else(type == "Markowitz-ML", NA_real_, tc),
      # r_tc = if_else(type == "Markowitz-ML", NA_real_, r_tc),
      # sr = if_else(type == "Markowitz-ML", NA_real_, sr),
      # obj = if_else(type == "Markowitz-ML", NA_real_, obj),
      order = type %>% factor(levels = c("One tuning layer", "Portfolio-ML", "Multiperiod-ML", "Static-ML", "Factor-ML", "Markowitz-ML",
                                         "Two tuning layers", "Multiperiod-ML*", "Static-ML*")),
      Method = if_else(type %in% c("One tuning layer", "Two tuning layers"), paste0("\\textbf{", as.character(type), "}"), paste0("\\hspace{0.5em}", as.character(type)))
    ) %>%
    arrange(order) %>%
    select(Method, "R"=r, "Vol."=sd, "SR$_\\text{gross}$"=sr_gross, "TC"=tc, "R-TC"=r_tc, "SR$_\\text{net}$"=sr, "Utility"=obj, "Turnover"=turnover_notional, "Lev."=inv) %>%
    xtable(digits = c(0, 2, 2, 2, 2, 3, 2, 2, 3, 2, 2)) %>%
    print.xtable(sanitize.text.function = identity, include.rownames = F, caption.placement = "top")
} else {
  # With all alternatives
  pf_summary %>%
    filter(type %in% c("Portfolio-ML", "Multiperiod-ML", "Static-ML", "Markowitz-ML", "Rank-ML", 
                       "Factor-ML", "Minimum Variance", "1/N", "Market",
                       "Multiperiod-ML*", "Static-ML*")) %>%
    rbind(
      data.table(type="One tuning layer"),
      data.table(type="Two tuning layers"), fill=T
    ) %>%
    mutate(
      # tc = if_else(type == "Markowitz-ML", NA_real_, tc),
      # r_tc = if_else(type == "Markowitz-ML", NA_real_, r_tc),
      # sr = if_else(type == "Markowitz-ML", NA_real_, sr),
      # obj = if_else(type == "Markowitz-ML", NA_real_, obj),
      order = type %>% factor(levels = c("One tuning layer", "Portfolio-ML", "Multiperiod-ML", "Static-ML", "Markowitz-ML", "Factor-ML", 
                                         "Rank-ML", "Minimum Variance", "1/N", "Market",
                                         "Two tuning layers", "Multiperiod-ML*", "Static-ML*")),
      Method = if_else(type %in% c("One tuning layer", "Two tuning layers"), paste0("\\textbf{", as.character(type), "}"), paste0("\\hspace{0.5em}", as.character(type)))
    ) %>%
    arrange(order) %>%
    select(Method, "R"=r, "Vol."=sd, "SR$_\\text{gross}$"=sr_gross, "TC"=tc, "R-TC"=r_tc, "SR$_\\text{net}$"=sr, "Utility"=obj, "Turnover"=turnover_notional, "Lev."=inv) %>%
    xtable(digits = c(0, 2, 2, 2, 2, 3, 2, 2, 3, 2, 2)) %>%
    print.xtable(sanitize.text.function = identity, include.rownames = F, caption.placement = "top")
}




# Probability of outperformance
prob_outperformance %>%
  mutate(
    prob_main_op = paste0(formatC(prob_main_op*100, digits = 1, format = "f"), "\\%")
  ) %>%
  pivot_wider(names_from = alt, values_from = prob_main_op) %>%
  select(all_of(c("main", main_types))) %>%
  rename(" "=main) %>%
  xtable(align = "llrrrrr") %>%
  print.xtable(sanitize.text.function = identity, include.rownames = F, caption.placement = "top")

# Correlation table
pf_cors_op <- formatC(pf_cors, digits = 2, format="f")
pf_cors_op[upper.tri(pf_cors_op, diag = F)]<-""
# pf_cors_op[-1, -ncol(pf_cors_op)] %>%
pf_cors_op %>%
  xtable(align="lccccc") %>%
  print.xtable(include.rownames = T, caption.placement = "top")

# Features information -----------------------------------------------
char_tbl <- cluster_labels[characteristic %in% features][order(cluster)][, .(n=1:.N,"Characteristic"=characteristic, "Cluster"=cluster)]
start <- ceiling(nrow(char_tbl)/3)
cbind(char_tbl[1:start], char_tbl[(start+1):(start*2)], rbind(char_tbl[(start*2+1):nrow(char_tbl)], data.table(n="", Characteristic="", Cluster = ""), data.table(n="", Characteristic="", Cluster = ""))) %>%
  xtable(align="llllllllll", caption = "tbl:chars") %>%
  print.xtable(include.rownames = F, caption.placement = "top")
