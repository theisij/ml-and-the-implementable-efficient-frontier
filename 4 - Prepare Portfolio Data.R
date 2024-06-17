# Add return predictions ---------------------------------
for (h in 1:settings$pf$hps$m1$K) {
  pred_data <- readRDS(paste0(get_from_path_model, "/model_", h, ".RDS")) |> 
    lapply(function(x) x$pred) |>  
    rbindlist() |>  
    select(id, eom, pred) |> 
    rename_with(~ paste0("pred_ld", h), .cols = "pred")
  chars <- pred_data[chars, on = .(id, eom)]
}

# Create lambda list -------------------------------------
lambda_dates <- unique(chars$eom)
lambda_list <- lambda_dates |> map(function(d) {
  x <- chars[eom==d, .(id, lambda)][order(id)]
  ids <- x$id
  x <- x$lambda
  names(x) <- ids
  return(x)
}) |> setNames(as.character(lambda_dates))
rm(lambda_dates)

# Important dates ----------------------
first_cov_date <- names(barra_cov) %>% as.Date() %>% min()
hp_years <- seq(from=settings$pf$dates$start_year, to=settings$pf$dates$end_yr)
start_oos <- settings$pf$dates$start_year+settings$pf$dates$split_years

dates_m1 <- seq.Date(from = settings$split$train_end+1, to = settings$split$test_end+1-months(1), by = "1 month")-1
dates_m2 <- seq.Date(from = first_cov_date+1+months(pf_set$lb_hor+1), to = settings$split$test_end+1-months(1), by = "1 month")-1
dates_oos <- seq.Date(from = as.Date(paste0(start_oos, "-", "01-01")), to = settings$split$test_end+1-months(1), by = "1 month")-1
dates_hp <- seq.Date(from = as.Date(paste0(min(hp_years), "-", "01-01")), to = settings$split$test_end+1-months(1), by = "1 month")-1
