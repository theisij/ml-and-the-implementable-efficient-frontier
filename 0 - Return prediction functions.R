# Split data 
data_split <- function(data, type, val_end, val_years, train_start, train_lookback, retrain_lookback, test_inc, test_end) {
  train_end <- val_end-years(val_years)
  train_start <- max(train_start, train_end - years(train_lookback))
  op <- list()
  op$val <- data[eom >= train_end & eom_pred_last <= val_end]
  op$train <- data[eom >= train_start & eom_pred_last <= train_end]
  op$train_full <- data[eom >= val_end-years(retrain_lookback) & eom_pred_last <= val_end]
  op$test <- data[eom >= val_end & eom < min(val_end+years(test_inc), test_end)]
  return(op)
}

# OLS --------------
ols_fit <- function(data, feat) {
  fit <- lm(paste0("ret_pred~",paste0(feat, collapse = "+")), data=data$train_full)
  pred <- data$test[, .(id, eom, eom_pred_last, pred=predict(fit, newdata = data$test))]
  list("fit"=fit, "pred"=pred)
}

# Random Fourier Features --------------------
rff <- function(X, p=NULL, g=NULL, W=NULL) { # P=number of features [MUST BE DIVISIBLE WITH 2!]
  # Draw random weights
  if (is.null(W)) {
    k <- ncol(X)
    W <- MASS::mvrnorm(n=p/2, mu=rep(0, k), Sigma = g*diag(k)) %>% t()
  } 
  X_new <- as.matrix(X) %*% W
  # Output
  list(W=W, X_cos=cos(X_new), X_sin=sin(X_new))
}

rff_hp_search <- function(data, feat, p_vec, g_vec, l_vec, seed) {
  # Search over g
  val_errors <- g_vec %>% lapply(function(g) {
    set.seed(seed)  # Ensures that draw is always the same irrespective of g
    print(paste0("g: ", formatC(g, digits=2, format="f"), " (", match(g, g_vec), " out of ", length(g_vec), ")"))
    # Create random features in training sets
    rff_train <- data$train[, feat, with=F] %>% rff(p = max(p_vec), g = g)
    rff_val <- data$val[, feat, with=F] %>% rff(W=rff_train$W)
    # Search over p
    err <- p_vec %>% lapply(function(p) {
      print(paste0("--> p: ", p, " (", match(p, p_vec), " out of ", length(p_vec), ")"))
      X_train <- p^(-0.5)*cbind(rff_train$X_cos[, 1:(p/2)], rff_train$X_sin[, 1:(p/2)])
      X_val <- p^(-0.5)*cbind(rff_val$X_cos[, 1:(p/2)], rff_val$X_sin[, 1:(p/2)])
      # Ridge fit
      fit <- glmnet(x = X_train, y = data$train$ret_pred, 
                    family = "gaussian", alpha = 0, lambda = l_vec, standardize = F)
      pred <- fit %>% predict(newx=X_val, type = "response", s = l_vec)
      1:length(l_vec) %>% lapply(function(i) {
        data.table(lambda=l_vec[i], mse=mean((pred[, i]-data$val$ret_pred)^2))
      }) %>%
        rbindlist() %>%
        mutate(p = p)
    }) %>% rbindlist() %>% mutate(g=g)
    list("err"=err, W=rff_train$W)
  })
  names(val_errors) <- g_vec
  # Optimal hps
  val_mse <- val_errors %>% map("err") %>% rbindlist()
  opt_hps <- val_mse[mse==min(mse)]
  opt_w <- val_errors[[as.character(opt_hps$g)]]$W[, 1:(opt_hps$p/2)]
  print(val_mse %>%
          mutate(
            lambda=if_else(log10(lambda)< -5, 0, lambda),
            g = paste0("g=", formatC(g, digits = 2, format = "f")),
            p = factor(p)
          ) %>%
          ggplot(aes(log10(lambda), mse, colour=p)) +
          geom_point(alpha=0.25) +
          geom_line() +
          # annotate(geom="label", x=Inf, y=Inf, label=paste0("Optimal: p=", opt_hps$p, ", g=", opt_hps$g, ", log10(l)=", round(log10(opt_hps$lambda), 2)), hjust=1, vjust=1) +
          facet_wrap(~g) +
          labs(y="Mean squared error"))
  # Re-fit on train-val data
  X_train_full <- data$train_full[, feat, with=F] %>% rff(W=opt_w)
  X_train_full <- opt_hps$p^(-0.5)*cbind(X_train_full$X_cos, X_train_full$X_sin)
  final_fit <- glmnet(x = X_train_full, y = data$train_full$ret_pred, 
                      family = "gaussian", alpha = 0, lambda = opt_hps$lambda, standardize = F)
  # Predict on test data
  X_test <- data$test[, feat, with=F] %>% rff(W=opt_w)
  X_test <- opt_hps$p^(-0.5)*cbind(X_test$X_cos, X_test$X_sin)
  pred_op <- data$test[, .(id, eom, eom_pred_last, pred=drop(predict(final_fit, newx = X_test, s=opt_hps$lambda)))]
  # Output
  list("fit"=final_fit, "pred"=pred_op, "hp_search"=val_mse, "W"=opt_w, "opt_hps"=opt_hps)
}

# Ridge HP search 
ridge_hp_search <- function(data, feat, vol_scale, lambdas) {
  fit <- glmnet(x = data$train[, feat, with=F] %>% as.matrix(), y = data$train$ret_pred, 
                family = "gaussian", alpha = 0, lambda = lambdas, standardize = T)
  pred <- fit %>% predict(newx=data$val[, feat, with=F] %>% as.matrix(), type = "response", s = lambdas)
  lambda_search <- 1:length(lambdas) %>% lapply(function(i) {
    data.table(lambda=lambdas[i], mse=mean((pred[, i]-data$val$ret_pred)^2))
  }) %>%
    rbindlist()
  print(lambda_search[log(lambda)!=-100] %>% ggplot(aes(log(lambda), mse)) + geom_point())
  lambda_opt <- lambda_search[mse==min(mse)]$lambda
  pred_val_op <- data$val[, .(id, eom, eom_pred_last, pred=pred[, match(lambda_opt, lambdas)])]
  # Re-fit to all training data
  final_fit <- glmnet(x = data$train_full[, feat, with=F] %>% as.matrix(), y = data$train_full$ret_pred, 
                      family = "gaussian", alpha = 0, lambda = lambda_opt, standardize = T) # Maybe a bad idea to re-standardize
  pred <- final_fit %>% predict(newx=data$test[, feat, with=F] %>% as.matrix(), s=lambda_opt)  # final_fit %>% broom::tidy() %>% filter(term %in% c(weekly_feat, "rvol_21d")) %>% arrange(-abs(estimate)) %>% ggplot(aes(reorder(term, abs(estimate)), abs(estimate))) + geom_col() + coord_flip()
  feat_imp <- final_fit %>% broom::tidy() %>% filter(term!="(Intercept)") %>% mutate(imp = frank(-abs(estimate)))
  pred_op <- data$test[, .(id, eom, eom_pred_last, pred=drop(pred))]
  if (vol_scale) {
    pred_val_op <- pred_val_op %>% mutate(pred_vol=pred, pred=pred_vol*data$val$rvol_m)
    pred_op <- pred_op %>% mutate(pred_vol=pred, pred=pred_vol*data$test$rvol_m)
  }
  # Output
  return(list("fit"=final_fit, "hp_search"=lambda_search, "l_opt"=lambda_opt, "pred"=pred_op, "pred_val"=pred_val_op, "feat_imp"=feat_imp))
}

# XGB ------------------
fit_xgb <- function(train, val, params, iter, es, cores, seed) { # train and val should be xgb.Dmatrix objects
  set.seed(seed)
  
  params_all <- list(
    objective = "reg:squarederror",
    base_score = 0,            
    eval_metric = "rmse", 
    booster = "gbtree",
    max_depth = params$tree_depth, 
    eta = params$learn_rate, 
    gamma = params$loss_reduction,  
    subsample = params$sample_size,        # Row subsampling
    colsample_bytree = params$mtry, # Column Subsampling
    min_child_weight = params$min_n,
    lambda = params$penalty
  )
  
  # Fit Model
  model <- xgb.train(
    data = train, 
    params = params_all,
    watchlist = list(train=train, val=val),
    nrounds = iter, 
    early_stopping_rounds = es,
    verbose = 0, 
    maximize = F,
    nthread = cores
  )
  return(model)
}


# XGB HP search
xgb_hp_search <- function(data, feat, vol_scale, hp_grid, iter, es, cores, seed) {
  # Find hyperparameters
  train <- xgb.DMatrix(data=as.matrix(data$train[, feat, with=F]), label=data$train$ret_pred)
  val <- xgb.DMatrix(data=as.matrix(data$val[, feat, with=F]), label=data$val$ret_pred)
  xgb_search <- 1:nrow(hp_grid) %>% lapply(function(j) {
    print(paste0("HP: ", j))
    xgb_fit <- fit_xgb(train = train, val = val, params = hp_grid[j, ], iter = iter, 
                       es = es, cores = cores, seed = seed)
    data.table(hp_no=j, val_rmse=xgb_fit$best_score, best_iter=xgb_fit$best_iteration)
  }) %>% rbindlist()
  print(xgb_search %>% ggplot(aes(hp_no, val_rmse)) + geom_point())
  best_hp_no <- xgb_search[val_rmse==min(val_rmse)][1]$hp_no # Had one case with identical rmse(!), arbitrarily just choose the first
  best_hp <- hp_grid[best_hp_no, ]
  best_iter <- xgb_search[hp_no==best_hp_no]$best_iter
  print("Best HP:")
  print(best_hp)
  print(paste("With", best_iter, "iterations"))
  # Re-fit to all training data
  train_all <- xgb.DMatrix(data=as.matrix(data$train_full[, feat, with=F]), label=data$train_full$ret_pred)
  final_fit <- fit_xgb(train = train_all, val = train_all, params = best_hp, iter = best_iter, 
                       es = NULL, cores = cores, seed = seed)
  # Feature importance
  set.seed(seed)
  shap_contrib <- predict(object=final_fit, newdata=as.matrix(sample_n(data$train_full[, feat, with=F], 10000)),predcontrib=T)
  global_imp <- abs(shap_contrib) %>% colMeans() %>% as_tibble(rownames = "char") %>% 
    filter(char != "BIAS") %>%
    mutate(
      rank = frank(-abs(value))
    )
  print(global_imp %>%
          filter(rank <= 20) %>% 
          ggplot(aes(reorder(char, abs(value)), abs(value))) + coord_flip() + geom_col()  + labs(y = "Global feature Importance") + theme(axis.title.y =  element_blank()))
  
  # Predictions
  pred <- final_fit %>% predict(newdata=data$test[, feat, with=F] %>% as.matrix())
  if (vol_scale) {
    pred_op <- data$test[, .(id, eom, pred_vol=drop(pred), pred=drop(pred)*rvol_m)]
  } else {
    pred_op <- data$test[, .(id, eom, pred=drop(pred))]
  }
  # Output
  list("fit"=final_fit, "best_hp"=best_hp, "best_iter"=best_iter, "hp_search"=xgb_search, "pred"=pred_op, "feat_imp"=global_imp)
}