# Estimate counterfactual predictions 
ret_cf <- 1:12 %>% map(.progress = "model iteration", function(h) {
  model <- readRDS(paste0(get_from_path_model, "/model_", h, ".RDS")) 
  # Prepare data
  pred_y <- data_ret[, .(id, eom, ret_pred=get(paste0("ret_ld", h)))]
  ret_cf_data <- pred_y[chars, on = .(id, eom)]
  preds <- model %>% map("pred") %>% rbindlist() %>% select(id, eom, pred)
  ret_cf_data <- ret_cf_data[preds, on = .(id, eom)][!is.na(ret_pred)]
  # Implement counterfactual
  c("bm", sort(clusters)) %>% map(progress=T, function(cls) {
    set.seed(settings$seed) # Set seed for reproducibility
    print(paste0("   ", cls)) 
    if (cls=="bm") {
      cf <- copy(ret_cf_data)
    } else {
      cf <- copy(ret_cf_data) %>% select(-c(pred)) # Counterfactual
      cf[, id_shuffle := sample(id, .N, replace=F), by = eom]
      cls_chars <- cluster_labels[cluster==cls & characteristic %in% features, characteristic]
      chars_data <- cf %>% select(all_of(c("id", "eom", cls_chars))) %>% rename("id_shuffle"=id)
      cf <- chars_data[cf[, (cls_chars) := NULL], on = .(id_shuffle, eom)]
      # Expected Returns
      for (m_sub in model) {
        if (!is.null(m_sub$pred)) {
          sub_dates <- unique(m_sub$pred$eom)
          cf_x <- cf[eom %in% sub_dates, features, with=F] %>% as.matrix()
          cf_new_x <- cf_x %>% rff(W=m_sub$W)
          cf_new_x <- m_sub$opt_hps$p^(-0.5)*cbind(cf_new_x$X_cos, cf_new_x$X_sin)
          cf[eom %in% sub_dates, pred := drop(m_sub$fit %>% predict(newx = cf_new_x, s = m_sub$opt_hps$lambda))]
        }
      }
    }
    cf[, .(cluster = cls, n = .N, mse = mean((pred-ret_pred)^2)), by = eom]
  }) %>% rbindlist() %>% mutate(h = h)
}) %>% rbindlist()
# Save
ret_cf |> fwrite(paste0(output_path, "/ret_cf.csv"))