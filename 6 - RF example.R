source("main.R")
# Settings ---
f <- function(x) {
  10*x+0.5*x^2-0.01*x^3-1.08^x+100*sin(x)
}
set.seed(1)
# Data
n <- 1000
x <- runif(n, -30, 70)
y_true <- f(x)
y <- y_true + rnorm(n, 0, 150)

# True function and OLS approximation ----
tibble(
  x=seq(-30, 70, by = 1), 
  y_true=f(x),
  y = y_true+rnorm(101, 0, 150)
) %>%
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  geom_line(aes(y=y_true)) +
  geom_smooth(method = "lm", se=F)

# Various RFF approximations ------------
set.seed(1)
ps <- seq(2^1, 2^9, by = 2)
lambdas <- c(exp(c(0:10)))
# Train/test split
train_idx <- sample(1:n, size = n/2, replace=F)
# Predict multiple lambdas and collect in nice tibble
pred_tidy <- function(fit, x_act, x, y, y_true, lambdas) {
  preds <- predict(fit, newx = x, s = lambdas)
  colnames(preds) <- log(lambdas)
  preds |> 
    as_tibble() |> 
    mutate(y = y, x = x_act, y_true=y_true) |> 
    pivot_longer(-c(x,y, y_true), names_to = "log_lambda", values_to = "pred")
}
# Simulate 
log_gs <- -3:2
data <- exp(log_gs) |> map(function(g) {
  rff_x <- as.matrix(x) %>% rff(p = max(ps), g = g)
  data <- ps %>% lapply(function(p) {
    rff_x_new <- p^(-0.5)*cbind(rff_x$X_cos[, 1:(p/2)], rff_x$X_sin[, 1:(p/2)])
    # Fit on training data
    x_train <- rff_x_new[train_idx, ]
    y_train <- y[train_idx]
    fit <- glmnet(x=x_train, y=y_train, alpha=0, lambda = lambdas)
    # Predict on train data 
    y_true_train <- y_true[train_idx]
    train_op <- fit |> 
      pred_tidy(x = x_train, x_act = x[train_idx], y = y_train, y_true = y_true_train, lambdas = lambdas) |> 
      mutate(split = "train")
    # Predict on test data (for R2)
    x_test <- rff_x_new[-train_idx, ]
    y_test <- y[-train_idx]
    y_true_test <- y_true[-train_idx]
    test_op <- fit |> pred_tidy(x = x_test, x_act = x[-train_idx], y = y_test, y_true = y_true_test, lambdas = lambdas) |> 
      mutate(split = "test")
    # Output
    rbind(train_op, test_op) |> 
      mutate(p = p)
  }) %>% bind_rows() |> mutate(g)
})  |> 
  bind_rows() |> 
  mutate(
    log_lambda = log_lambda |> factor(levels = log(lambdas)),
    g_name = paste0("log(g): ", log(g)) |> factor(levels = paste0("log(g): ", log_gs))
  )

# Figure: See fit ------
# By ridgeless
(output$rff_specific0 <- data |> 
   filter(split=="train") |> 
   filter(log_lambda %in% c(0) & g %in% c(1), p %in% 2^(1:6)) |>
   pivot_longer(c(y_true, pred)) |>
   # filter(log_lambda %in% c(2:5) & g %in% c(1)) |>
   mutate(p = paste0("P=", p) %>% factor(levels = paste0("P=", ps))) |>
   mutate(y = if_else(name == "y_true", y, NA_real_)) |>
   mutate(name = case_when(
     name == "pred" ~ "RF prediction",
     name == "y_true" ~ "True function"
   )) |> 
   ggplot(aes(x, y = value, colour = name)) + 
   geom_point(aes(y=y), colour = "grey", alpha=0.5) +
   geom_line() + 
   # geom_line(aes(y=y_true), colour="black") +
   # geom_line(aes(y=pred)) +
   facet_wrap(~p) +
   labs(y="Value") +
   theme(legend.position = "top", legend.title = element_blank()))

# By lambda
(output$rff_specific1 <- data |> 
  filter(split=="train") |> 
  filter(log_lambda %in% c(0, 6, 10) & g %in% c(1), p %in% 2^(1:6)) |>
  # filter(log_lambda %in% c(2:5) & g %in% c(1)) |>
  mutate(p = paste0("P=", p) %>% factor(levels = paste0("P=", ps))) |>
  ggplot(aes(x, y = pred, colour = factor(log_lambda))) + 
  geom_point(aes(y=y), colour = "grey", alpha=0.5) +
  geom_line(aes(y=y_true), colour="black") +
  geom_line(aes(y=pred)) +
  facet_wrap(~p) +
  labs(colour="Log(lambda)")) 
# By g
(output$rff_specific2 <- data |> 
  filter(split=="train") |> 
  filter(log_lambda %in% c(0), p %in% 2^(1:6)) |>
  mutate(p = paste0("P=", p) %>% factor(levels = paste0("P=", ps))) |>
  ggplot(aes(x, y = pred, colour = factor(log(g)))) + 
  geom_point(aes(y=y), colour = "grey", alpha=0.5) +
  geom_line(aes(y=y_true), colour="black") +
  geom_line(aes(y=pred)) +
  facet_wrap(~p) +
  labs(colour="Log(g)"))

# Figure: R2 by g, p, and lambda ----
(output$rff_overview <- data |> 
  filter(split=="test") |> 
  group_by(g, p, log_lambda) |>
  summarise(
    r2 = 1 - sum((y - pred)^2) / sum((y - mean(y))^2) 
  ) |> 
  mutate(g = paste0("log(g): ", log(g)) |> factor(levels = paste0("log(g): ", log_gs))) |> 
  ggplot(aes(x=p, y=r2, colour=factor(log_lambda))) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~g) +
  labs(x = "Number of random features, p", colour = "Log(lambda)", y = "Out-of-sample R2"))




# OLD SCRIBBLES -------------------------
if (FALSE) {
  # Simple example -------------------------------------------------
  x <- seq(0, 1, 0.01)
  y <- case_when(
    x <= 0.25 ~ -0.005,
    x <= 0.50 ~ 0,
    x <= 0.75 ~ 0.005,
    x > 0.75 ~ 0.02,
  )
  y <- case_when(
    x <= 0.75 ~ 0.005,
    x > 0.75 ~ 0.005 + (x-0.75)*0.1
  )
  y <- -0.01+x*0.02
  y <- -0.01+0.01+x + 1*x^3
  y_true <- -0.01+20^x
  y_true <- -0.005+0.001*x+0.01*(1 / (1 + exp(-10*(x-0.1))))
  # x <- seq(-30, 70, by = 1)
  # y_true <- 10*x+0.5*x^2-0.01*x^3-1.08^x#+100*sin(x)
  # y <- y_true + rnorm(length(y_true), 0, 150)
  
  # True function and OLS approximation
  y_true <- -10-100*x+300*x^2-1500*(x-0.5)^3+50^x
  y <- y_true + rnorm(length(y_true), 0, 10)
  tibble(
    x=x, 
    y=y
  ) %>%
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_smooth(method = "lm", se=F)
  
  # RFF approximation
  set.seed(settings$seed_no)
  ps <- 1:6
  
  data <- c(settings$rff$g_vec[1], 1) |> map(function(g) {
    rff_x <- as.matrix(x) %>% rff(p = 2^max(ps), g = g)
    data <- ps %>% lapply(function(i) {
      p <- 2^i
      rff_x_new <- p^(-0.5)*cbind(rff_x$X_cos[, 1:(2^i/2)], rff_x$X_sin[, 1:(2^i/2)])
      rff_reg <- lm(y~-1+rff_x_new)
      tibble(
        p = p,
        y_true = y_true,
        y = y,
        x = x, 
        preds = drop(predict(rff_reg)),
        ols_pred = drop(predict(lm(y~x)))
      )
    }) %>% bind_rows() |> mutate(g)
  })  |> bind_rows()
  data %>%
    pivot_longer(-c(g, p, x)) %>%
    mutate(
      p = paste0("P=", p) %>% factor(levels = paste0("P=", 2^ps)),
      name = case_when(
        name == "preds" ~ "RFF prediction",
        name == "y_true" ~ "Truth",
        name == "y" ~ "y",
        name == "ols_pred" ~ "OLS prediction"
      )
    ) %>%
    filter(name != "OLS prediction") |> 
    ggplot(aes(x, value, colour=name, linetype=factor(g))) +
    # geom_point(alpha=0.5) +
    geom_line() +
    facet_wrap(~p, scales = "free_y") +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  data |> 
    mutate(p = paste0("P=", p) %>% factor(levels = paste0("P=", 2^ps))) |>
    ggplot(aes(x, y = preds, colour = factor(round(g,2)))) + 
    geom_point(aes(y=y), colour = "grey") +
    geom_line(aes(y=y_true), colour="black") +
    geom_line(aes(y=preds)) +
    facet_wrap(~p) +
    labs(colour="RFF with g") 
  
  
  
  
  # Sequential plots
  rff_x_2 <- 2^(-0.5)*cbind(rff_x$X_cos[, 1], rff_x$X_sin[, 1])
  rff_reg2 <- lm(y~-1+rff_x_2)
  rff_x_4 <- 4^(-0.5)*cbind(rff_x$X_cos[, 1:2], rff_x$X_sin[, 1:2])
  rff_reg4 <- lm(y~-1+rff_x_4)
  
  
  
  rbind(
    tibble(x = x, y = rff_reg2$coefficients[1]*rff_x_2[, 1], name = "cos1", p = 2),
    tibble(x = x, y = rff_reg2$coefficients[2]*rff_x_2[, 2], name = "sin1", p = 2),
    tibble(x = x, y = rff_reg4$coefficients[1]*rff_x_4[, 1], name = "cos1", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[2]*rff_x_4[, 2], name = "cos2", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[3]*rff_x_4[, 3], name = "sin1", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[4]*rff_x_4[, 4], name = "sin2", p = 4)
  ) %>%
    ggplot(aes(x, y, colour=name)) +
    geom_line() +
    facet_wrap(~p, scales = "free_y")
  
  rbind(
    tibble(x = x, y = rff_reg2$coefficients[1]*rff_x_2[, 1], name = "cos1", p = 2),
    tibble(x = x, y = rff_reg2$coefficients[2]*rff_x_2[, 2], name = "sin1", p = 2),
    tibble(x = x, y = rff_reg4$coefficients[1]*rff_x_4[, 1], name = "cos1", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[2]*rff_x_4[, 2], name = "cos2", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[3]*rff_x_4[, 3], name = "sin1", p = 4),
    tibble(x = x, y = rff_reg4$coefficients[4]*rff_x_4[, 4], name = "sin2", p = 4)
  ) %>% 
    group_by(p, x) %>% 
    summarise(pred_y = sum(y)) %>% 
    ggplot(aes(x, pred_y, colour=factor(p))) + 
    geom_line()
  
  test <- cbind(rff_x$X_cos, rff_x$X_sin)
  colnames(test) <- c(paste0("cos", 1:ncol(rff_x$X_cos)), paste0("sin", 1:ncol(rff_x$X_sin)))
  test <- cbind(x=x, test) 
  
  as_tibble(test) %>%
    pivot_longer(-x) %>%
    mutate(
      type = substr(name, 1, 3),
      no = as.integer(substr(name, 4, 4))
    ) %>%
    ggplot(aes(x, value, colour=factor(no))) +
    geom_line() +
    facet_wrap(~type, scales="free_y")
  
  tibble(x = x, cos_x = cos(-x), sin_x = sin(-x)) %>% 
    pivot_longer(-x) %>% 
    ggplot(aes(x, value,colour=name)) + 
    geom_line()
  
  
  # One independent variable ---------------------------------------
  # True model ----------------------
  x <- seq(-30, 70, by = 1)
  y <- 10*x+0.5*x^2-0.01*x^3-1.08^x#+100*sin(x)
  
  tibble(x=x, y=y) %>%
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se=F) +
    labs(title = "y=10x+0.5x^2-0.01x^3-1.08^x+100*sin(x)")
  
  # RFF approximation --------------
  set.seed(settings$seed_no)
  ps <- 1:6
  g_vec = exp(-4)
  data <- ps %>% lapply(function(p) {
    rff_x <- as.matrix(x) %>% rff(p = 2^p, g = 1)
    rff_x <- cbind(rff_x$X_cos, rff_x$X_sin)
    rff_reg <- lm(y~rff_x)
    tibble(
      p = p,
      y = y,
      x = x, 
      preds = drop(predict(rff_reg)),
      ols_pred = drop(predict(lm(y~x)))
    )
  }) %>% bind_rows()
  
  data %>%
    pivot_longer(-c(p, x)) %>%
    mutate(
      p = paste0("P=", 2^p) %>% factor(levels = paste0("P=", 2^ps)),
      name = case_when(
        name == "preds" ~ "RFF prediction",
        name == "y" ~ "Truth",
        name == "ols_pred" ~ "OLS prediction"
      )
    ) %>%
    ggplot(aes(x, value, colour=name)) +
    # geom_point(alpha=0.5) +
    geom_line() +
    facet_wrap(~p, scales = "free_y") +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    ) +
    labs(title = "y=10x+0.5x^2-0.01x^3-1.08^x+100*sin(x)")
  
  # Two variables -----------------------------
  # True model ----------------------
  x <- seq(-30, 70, by = 1)
  z <- c(rep(-1, length(x)), rep(1, length(x)))
  x <- rep(x, 2)
  y <- z*(10*x+0.5*x^2-0.01*x^3-1.08^x)#+100*sin(x))
  X <- cbind(z, x)
  
  tibble(x=x, y=y, z=z) %>%
    ggplot(aes(x=x, y=y, colour = factor(z))) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se=F) +
    labs(title = "y=10x+0.5x^2-0.01x^3-1.08^x+100*sin(x)")
  
  # RFF approximation --------------
  set.seed(settings$seed_no)
  ps <- 1:7
  rff_x <- as.matrix(X) %>% rff(p = 2^max(ps), g = 1)
  data <- ps %>% lapply(function(i) {
    rff_x_new <- cbind(rff_x$X_cos[, 1:(2^i/2)], rff_x$X_sin[, 1:(2^i/2)])
    tibble(
      p = 2^i,
      y = y,
      z = z, 
      x = x, 
      preds = drop(predict(lm(y~rff_x_new))),
      ols_pred = drop(predict(lm(y~X)))
    )
  }) %>% bind_rows()
  
  (output$rff <- data %>%
      filter(p %in% 2^c(1, 5, 6)) %>%
      pivot_longer(-c(p, x, z)) %>%
      mutate(
        p = paste0("P=", p) %>% factor(levels = paste0("P=", 2^ps)),
        z = paste0("Z=", z),
        name = case_when(
          name == "preds" ~ "RF prediction",
          name == "y" ~ "Truth",
          name == "ols_pred" ~ "OLS prediction"
        )
      ) %>%
      ggplot(aes(x, value, colour=name)) +
      # geom_point(alpha=0.5) +
      geom_line() +
      facet_wrap(factor(z)~p, ncol=3) +
      theme(
        legend.position = "top",
        legend.title = element_blank()
      ) +
      labs(x = "X", y = "Y"))#+
  # labs(title = "y=z[10x+0.5x^2-0.01x^3-1.08^x+100*sin(x)]")
  
  # data %>%
  #   filter(p %in% 2^(4:6)) %>%
  #   pivot_wider(names_from = p, values_from = preds, names_prefix = "RFF") %>%
  #   pivot_longer(-c(z, x)) %>%
  #   ggplot(aes(x, value, colour=name)) +
  #   geom_line() +
  #   facet_wrap(~z)
  
  # Check RFF variation --------
  x <- seq(0, 1, 0.01)
  rff_x <- as.matrix(x) %>% rff(p = 2^4, g = exp(4))
  cbind(rff_x$X_cos, rff_x$X_sin) |> 
    as_tibble() |> 
    mutate(x = x) |> 
    pivot_longer(-x) |> 
    ggplot(aes(x, value, colour=name)) +
    geom_point() +
    facet_wrap(~name, scales = "free_y")
  
  x1 <- runif(n=115)
  sin(x1 %*% rnorm(n=115, mean=0, sd=exp(-4)))
  
  
  
}
