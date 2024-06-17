start_time <- proc.time()
  
models <- 1:nrow(search_grid) %>% lapply(function(i) {
  # Prepare y variable
  h <- search_grid[i, ]$horizon %>% unlist()
  pred_y <- data_ret[, paste0("ret_ld", h), with=F] %>% rowMeans()
  pred_y <- data_ret[, .(id, eom, eom_pred_last=eom+1+months(max(h))-1, ret_pred=pred_y)]
  data_pred <- pred_y[chars[valid==T], on = .(id, eom)]
  
  print(paste0("horizons: ", list(h)))
  
  # Fit model
  if (settings$split$model_update_freq == "once") {
    val_ends <- settings$split$train_end
    test_inc <- 1000
  }
  if (settings$split$model_update_freq == "yearly") {
    val_ends <- seq.Date(from = settings$split$train_end, to = settings$split$test_end, by = "1 year")
    test_inc <- 1
  }
  if (settings$split$model_update_freq == "decade") {
    val_ends <- seq.Date(from = settings$split$train_end, to = settings$split$test_end, by = "10 years")
    test_inc <- 10
  }
  system.time({op <- val_ends %>% sapply(simplify = F, USE.NAMES = T, function(val_end) {
    print(val_end)
    train_test_val <- data_pred %>% data_split(type = settings$split$model_update_freq, val_end = val_end, val_years = settings$split$val_years, train_start = settings$screens$start, 
                                               train_lookback = settings$split$train_lookback, retrain_lookback = settings$split$retrain_lookback,
                                               test_inc = test_inc, test_end = settings$split$test_end)
    # Fit models
    print(system.time(model_op <- train_test_val %>% rff_hp_search(feat = features, p_vec=settings$rff$p_vec, g_vec=settings$rff$g_vec, l_vec=settings$rff$l, seed=settings$seed_no)))
    
    return(model_op)
  })})
  op %>% saveRDS(paste0(output_path, "/model_", h,".RDS"))
  # op %>% saveRDS(paste0(output_path, "/models/model_", settings$split$model_update_freq, "_", min(h), "_", max(h), ".RDS"))
  return(op)
})
# Save output ------
# names(models) <- search_grid$name

# models %>% saveRDS(paste0(output_path, "/models/model_full.RDS"))

proc.time()-start_time # 15 hours with all stocks, xgb, and yearly updates, 

