# Markowitz-ML - Base case ---------------------
cf_clusters <- c("bm", clusters)
tpf_cf_wealth <- wealth_func(wealth_end = 0, end = settings$split$test_end, market = market, risk_free = risk_free)
er_models <- readRDS(paste0(get_from_path_model, "/model_1.RDS"))
tpf_cf_base <- cf_clusters |> map(.progress = T, function(cf_cluster) {
  chars[valid==T] |> tpf_cf_fun(
    cf_cluster = cf_cluster, 
    er_models = er_models,
    dates = dates_oos, 
    cluster_labels = cluster_labels,
    cov_list = barra_cov, 
    gamma_rel = 100,                 # Use higher gamma to make TPF less extreme (and increase realized utility) 
    wealth = tpf_cf_wealth,
    seed = settings$seed_no
  )
}) |> rbindlist()
# Save
tpf_cf_base |> fwrite(paste0(output_path, "/tpf_cf_base.csv"))

# tpf_cf_base[, .(mean = mean(r)*12, sd = sd(r)*sqrt(12), u = (mean(r)-pf_set$gamma_rel/2*var(r))*12), by = cluster][, sr := mean/sd][order(sr)]

# Portfolio-ML - Base case --------------------
# Load PFML base case model, for its HPs
base_path <- "Data/Generated/Portfolios/Base/"
pfml <- readRDS(paste0(base_path, list.files(base_path), "/portfolio-ml.RDS"))
# All clusters are implemented for the base case
cf_clusters <- c("bm", clusters)
pfml_cf_base <- cf_clusters |> map(.progress = T, function(cf_cluster) {
  chars[valid==T] |> pfml_cf_fun(
    cf_cluster = cf_cluster, 
    pfml_base = pfml, 
    dates = dates_oos, 
    cov_list = barra_cov, 
    scale = settings$pf_ml$scale, 
    orig_feat = settings$pf_ml$orig_feat, 
    gamma_rel = pf_set$gamma_rel, 
    wealth = wealth, 
    risk_free = risk_free, 
    mu = pf_set$mu, 
    iter = 10,
    seed = settings$seed_no
  )
}) |> rbindlist()
# Save
pfml_cf_base |> fwrite(paste0(output_path, "/pfml_cf_base.csv"))