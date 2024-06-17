# Run this code to fit models in parallel on a slurm HPC
source("Main.R")

# Submit a config file with slurm job? -----------
if (TRUE) {
  # Prepare for slurm to submit configuration files flexibly
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- args[1]  # Get the first argument, which is the config file name
  
  config_params <- read_config(config_file)
  # Time notes ----
  # Running all three methods on quantiles splits of the stocks took roughly 20 hours
  # Runing just Static on all stocks took 3.5 hours, PFML took 65 hours, and Multiperiod-ML likely takes longer....
  
} else {
  config_params <- list(
    "size_screen"="perc_low50_high100_min50",  #"top100"
    "wealth" = 1e10,
    "gamma_rel" = 10,
    "industry_cov" = T,
    "update_mp" = F,
    "update_base" = F,
    "update_fi_base" = F,
    "update_fi_ief" = F,
    "update_fi_ret" = F
  )
}

settings$screens$size_screen <- config_params$size_screen
settings$cov_set$industries <- config_params$industry_cov
pf_set$wealth <- config_params$wealth
pf_set$gamma_rel <- config_params$gamma_rel


# Run settings --------------------
get_from_path_model <- "Data/Generated/Models/20240424-all"  # From which directory should the data be loaded? (if update$x==F)
run_sub <- F  # T when testing code, F otherwise

# Create folder to save output --------------------
folder_naming <- function(config_params) {
  paste0(
    "Data/Generated/Portfolios/", format(Sys.time(), "%Y%m%d-%H%m"), 
    "_WEALTH", config_params$wealth,
    "_GAMMA", config_params$gamma_rel,
    "_SIZE", config_params$size_screen,
    "_IND", config_params$industry_cov
  )
}
output_path <- config_params |> folder_naming()
if (!dir.exists(output_path)) {
  # Create relevant directories
  dir.create(output_path) # Overall
  # Save Main.R and settings file to new directory
  file.copy("Main.R", paste0(output_path, "/Main.R"))
  file.copy("slurm_build_portfolios.R", paste0(output_path, "/slurm_build_portfolios.R"))
  settings |> saveRDS(paste0(output_path, "/settings.RDS"))
  pf_set |> saveRDS(paste0(output_path, "/pf_set.RDS"))
}

# Run code --------------------
tic("Total run time")

source("1 - Prepare Data.R", echo = T)
# source("2 - Fit Models.R", echo = T)  # Loaded in prepare portfolio data
source("3 - Estimate Covariance Matrix.R", echo = T)
source("4 - Prepare Portfolio Data.R", echo = T)
# Base case
if (config_params$update_base) {
  tic("Base case:")
  source("5 - Base case.R", echo = T)  
  toc()
}
# Feature importance - base case
if (config_params$update_fi_base) {
  tic("Feature importance - base:")
  source("5 - Feature importance base.R", echo = T)
  toc()
}
# Feature importance - IEF
if (config_params$update_fi_ief) {
  tic("Feature importance - IEF:")
  source("5 - Feature importance IEF.R", echo = T)
  toc()
}
# Feature importance - Expected return models
if (config_params$update_fi_ret) {
  tic("Feature importance - returns:")
  source("5 - Feature importance ret.R", echo = T)
  toc()
}

toc()
