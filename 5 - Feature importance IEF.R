# Portfolio-ML - IEF --------------------
# Only some clusters are implemented for full IEF
ief_path <- "Data/Generated/Portfolios/IEF/"
ief_cf_clusters <- c("quality", "value", "momentum", "short_term_reversal")
pfml_cf_ief <- settings$ef$gamma_rel |> map(.progress = T, function(gamma_rel) {
  # Find hyper-parameters for specific gamma
  x <- paste0("20240514-20_WEALTH1e+10_GAMMA", gamma_rel, "_SIZEperc_low50_high100_min50_INDTRUE")
  pfml <- readRDS(paste0(ief_path, x, "/portfolio-ml.RDS"))
  # Implement for each cluster
  pfml_cf_base <- ief_cf_clusters |> map(.progress = T, function(cf_cluster) {
    chars[valid==T] |> pfml_cf_fun(
      cf_cluster = cf_cluster, 
      pfml_base = pfml, 
      dates = dates_oos, 
      cov_list = barra_cov, 
      scale = settings$pf_ml$scale, 
      orig_feat = settings$pf_ml$orig_feat, 
      gamma_rel = gamma_rel, 
      wealth = wealth, 
      risk_free = risk_free, 
      mu = pf_set$mu, 
      iter = 10,
      seed = settings$seed_no
    )
  }) |> rbindlist() |> mutate(gamma_rel = gamma_rel)
}) |> rbindlist()
# Save
pfml_cf_ief |> fwrite(paste0(output_path, "/pfml_cf_ief.csv"))
