library(tidyverse)
library(data.table)

# Layout settings ----------------------------------------------
theme_set(theme_classic(base_size = 13))
colours_theme <- c("#0C6291", "#A63446", RColorBrewer::brewer.pal(8, "Dark2"), 
                   "darkslategrey", "blue3", "red3", "purple2", "yellow2", "aquamarine",
                   "grey", "salmon", "antiquewhite", "chartreuse") 
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = colours_theme)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = colours_theme)
}

# Aggregate results -------------------------------------------
files <- list.files("simulations/results")
files <- files[str_detect(files, "stocks500") & str_detect(files, "-05-18")] #  | str_detect(files, "-05-19")

results <- files |> map(function(f) {
  fread(paste0("simulations/results/", f))
}) |> rbindlist()
if (FALSE) {
  # Forgot to add theta in run on april 17th
  results <- 1:length(files) |> map(function(i) {
    fread(paste0("simulations/results/", files[i])) |> mutate(theta1=search_grid$theta1[i], theta2=search_grid$theta2[i])
  }) |> rbindlist()
}

# Results -----------------------------------------------------
results[, e_var_adj := (r-mean(r))^2, by=.(run, seed, type, w, theta1, theta2, n)]
results[, utility_t := r-tc-0.5*e_var_adj*10]

results_overall <- results[, .(
  n_obs = .N,
  inv = mean(inv),
  shorting = mean(shorting),
  turnover_notional = mean(turnover),
  r = mean(r)*12,
  sd = sd(r)*sqrt(12),
  sr_gross = mean(r)/sd(r)*sqrt(12),
  tc = mean(tc)*12,
  r_tc = mean((r-tc))*12,
  sr = mean(r-tc)/sd(r)*sqrt(12),
  obj = (mean(r)-0.5*var(r)*10-mean(tc))*12#,
  # obj_exp = (mean(e_r)-0.5*mean(e_var)*10-mean(tc))*12
), by = .(run, seed, type, w, theta1, theta2, n)]
results_overall <- results_overall[type != "Static-ML"]
results_overall[type=="Multiperiod-ML*", type := "Multiperiod-ML"] # No tuning, so no need for "*"

# Utility ----------------
(output$simulations <- results_overall |> 
  filter(!(theta1==0 & theta2==0.95)) |>
  # filter(w != 1e8) |> # Effectively zero cost at this level (need to re-check with new data)
  group_by(run, w, type, theta1, theta2, n) |>
  summarise(
    n = n(),
    obj = median(obj),
    high = max(obj),
    low = min(obj)
  ) |>
  mutate(
    theta = paste0("theta1=", theta1, ", theta2=", theta2)
  ) |> 
  # filter(type!="Static-ML*") |>
  # filter(type!="Static-ML") |>
  ggplot(aes(x=factor(w/1e9), y=obj, colour=type, group=type)) +
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0, linetype="dashed", colour="black") +
  coord_cartesian(ylim = c(-0.01, NA )) + 
  facet_wrap(~theta, nrow = 1, scales="free_y") +
  labs(x = "Wealth by 2020 ($Billion)", y = "Realized utility") + 
  theme(
    legend.position = "top",
    legend.title = element_blank(),
  ))

if (FALSE) {
  # Old scribbles ---------------
  # Relevant range: One fast, one slow
  results_overall |> 
    filter(theta1==0 & theta2==0.95) |> 
    group_by(seed, w, type, theta1, theta2, n) |>
    group_by(run, w, type, theta1, theta2, n) |>
    summarise(
      n = n(),
      obj = median(obj),
      high = max(obj),
      low = min(obj)
    ) |>
    mutate(
      theta = paste0("Theta1=", theta1, ", theta2=", theta2)
    ) |> 
    # filter(type!="Static-ML*") |>
    # filter(type!="Static-ML") |>
    ggplot(aes(x=factor(w), y=obj, colour=type, group=type)) +
    geom_point() +
    geom_line() + 
    coord_cartesian(ylim = c(-0.01, NA )) + 
    facet_wrap(~theta, ncol = 2, scales="free_y") +
    labs(x = "Wealth", y = "Realized utility OOS") + 
    theme(
      legend.position = "top",
      legend.title = element_blank(),
    )
  
  results_overall |> 
    # group_by(w, type, theta, n) |>
    # summarise(
    #   obj = median(obj),
    #   high = max(obj),
    #   low = min(obj)
    # ) |> 
    # filter(type!="Static-ML*") |>
    # filter(type!="Static-ML") |>
    ggplot(aes(x=factor(w), y=obj, colour=type, group=type)) +
    geom_point(alpha=0.5, size=1) +
    geom_line() + 
    coord_cartesian(ylim = c(-0.2, NA )) + 
    facet_wrap(~theta, ncol = 3, scales="free_y")
  
  # Turnover ---------------
  results_overall |> 
    ggplot(aes(x=factor(w), y=turnover_notional, colour=type, group=type)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~theta) +
    labs(x = "Wealth", y = "Realized turnover") + 
    theme(
      legend.position = "top",
      legend.title = element_blank(),
    )
  
  
  # Gross Sharpe ratio ------------
  results_overall |> 
    ggplot(aes(x=factor(w), y=sr_gross, colour=type, group=type)) +
    geom_point() +
    geom_line() + 
    facet_wrap(~theta) +
    labs(x = "Wealth", y = "Realized gross Sharpe ratio") + 
    theme(
      legend.position = "top",
      legend.title = element_blank(),
    )
  
  results[n==500 & theta==0.05 & w==1e11][, cum_u := cumsum(utility_t)] |> 
    ggplot(aes(eom_ret, cum_u, colour=type)) + 
    geom_point() + 
    geom_line()
  
  results[n==100 & theta==0.95 & w==1e11][, cum_u := cumsum(r)] |> 
    ggplot(aes(eom_ret, cum_u, colour=type)) + 
    geom_point() + 
    geom_line()
  
  
  # Test: Expected utility from trade ----------------------------
  # For Static and TPF, compute expected change in utility from trade
  start_end_u_fun <- function(ws) {
    ds <- unique(ws$eom)
    ds |> map(function(d) {
      w <- ws[eom==d]
      er <- data_tc_list[[as.character(d)]]$er
      sigma_gam <- data_tc_list[[as.character(d)]]$sigma_gam
      lambda <- data_tc_list[[as.character(d)]]$lambda
      wealth_t <- wealth[eom == d]$wealth
      start <- w$w_start
      end <- w$w
      u_start <- start %*% er - 1/2*(t(start) %*% sigma_gam %*% start)
      u_end <- end %*% er - 1/2*(t(end) %*% sigma_gam %*% end)
      tc <- wealth_t/2 * (end-start) %*% lambda %*% (end-start)
      data.table(eom=d, u_start=drop(u_start), 
                 u_end=drop(u_end), 
                 tc=drop(tc))
    }) |> rbindlist()
  }
  
  static$w |> start_end_u_fun() |> mutate(trade_imp = u_end-u_start-tc)
  tpf$w |> start_end_u_fun() |> mutate(trade_imp = u_end-u_start-tc)
  
  # Analysis -----------------------------------------------------
  # Mean-variance efficient portfolio OOS
  data_tc[eom %in% dates_oos, .(rr = sum(ret_ld1*w_tpf), er = sum(mu*w_tpf)), by=eom][, .(
    n = .N,
    mean_er = mean(er)*12,
    mean_rr = mean(rr)*12,
    sd_rr = sd(rr)*sqrt(12)
  )][, sr := mean_rr/sd_rr][]
  
  # Mean-variance efficient portfolio FULL
  data_tc[, .(rr = sum(ret_ld1*w_tpf), er = sum(mu*w_tpf)), by=eom][, .(
    n = .N,
    mean_er = mean(er)*12,
    mean_rr = mean(rr)*12,
    sd_rr = sd(rr)*sqrt(12)
  )][, sr := mean_rr/sd_rr][]
  
  data_tc[, .(rr = sum(ret_ld1*w_tpf), er = sum(mu*w_tpf)), by=eom][, .(
    n = .N,
    mean_er = mean(er)*12,
    mean_rr = mean(rr)*12,
    sd_rr = sd(rr)*sqrt(12)
  )][, sr := mean_rr/sd_rr][]
  
  
  data_tc[, .(rr = sum(r*w_tpf), er = sum(mu*w_tpf)), by=t][, cumret := cumsum(rr)] |> 
    ggplot(aes(t, cumret)) +
    geom_point()
  # SR by stock (SR increase in x because of constant ivol)
  data_tc[, .(
    sr = mean(sr),
    x = mean(x)
  ), by = i][order(x)]
  
  
  data_tc[id <=2 & year(eom) %in% 2019:2020] |> ggplot(aes(eom, mu*12, colour=factor(id))) + geom_line() + geom_point()
  
  
  # Expected returns -------------
  ers <- c(0, 0.95) |> map(function(theta) {
    rf <- 0
    risk_free <- data.table(eom=dates_full, rf=rf)
    # Inputs to prepare_data_list ----
    wealth <- data.table(
      eom=dates_full, 
      wealth=1e10, 
      mu_ld1 = pf_set$mu)
    # Prepare data 
    data_list <- sim_prep_data(
      t = t,
      n = 10,
      theta = theta,
      dates_full = dates_full, 
      rf = rf,
      seed = set$seed_no
    )
    data_list$data |> mutate(theta=theta)
  }, .progress = T) |> rbindlist()
  
  ers[id==1] |>
    ggplot(aes(eom, pred_ld1, colour=factor(id))) + 
    geom_line() +
    facet_wrap(~theta)
  
  ers[, .(ar1=cor(pred_ld1, lag(pred_ld1, 1), use="complete.obs")), by = .(id, theta)] |> 
    group_by(theta) |>
    summarise(ar1=mean(ar1)) |> 
    ggplot(aes(theta, ar1)) +
    geom_point() +
    geom_abline(intercept=0, slope=1, linetype="dashed")
  
  # Trade aggresiveness of TPF -------------------------------
  tpf_ws <- c(0, 0.95) |> map(function(theta) {
    rf <- 0
    risk_free <- data.table(eom=dates_full, rf=rf)
    # Inputs to prepare_data_list ----
    wealth <- data.table(
      eom=dates_full, 
      wealth=1e10, 
      mu_ld1 = pf_set$mu)
    # Prepare data 
    data_list <- sim_prep_data(
      t = t,
      n = 100,
      theta = theta,
      dates_full = dates_full, 
      rf = rf,
      seed = set$seed_no
    )
    # TPF 
    data_tc_list <- data_list$data[valid==T] %>% prepare_data_list(dates=dates_oos, wealth=wealth, risk_free=risk_free,
                                                                   barra_cov = data_list$barra_cov, gamma_rel = pf_set$gamma_rel)
    tpf <- data_list$data %>% tpf_implement(data_list = data_tc_list, dates = dates_oos, wealth = wealth)
    # Add expected returns
    data_list$data[, .(id, eom, x1, x2, pred_ld1)][tpf$w, on = .(id, eom)] |> mutate(theta=theta)
  }, .progress = T) |> rbindlist()
  
  tpf_ws[id<=3] |>  
    pivot_longer(c(w, x1, pred_ld1)) |> 
    ggplot(aes(eom, value, colour=factor(id))) +
    geom_line() +
    facet_wrap(name~theta, scales="free_y", ncol = 2)
  
  tpf_ws[, .(
    turnover = sum(abs(w-w_start))
  ), by = .(theta, eom)][, .(turnover = mean(turnover)), by = theta]
}

