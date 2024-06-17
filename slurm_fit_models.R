# Run this code to fit models in parallel on a slurm HPC
# Note: when distributing 12 models across 4 instances, request 20 hours and 150gb memory on each instance 
source("Main.R")

# Run settings --------------------
settings$screens$size_screen <- "all"
settings$screens$nyse_stocks <- F # Also include non-NYSE stocks when fitting expected return models
run_sub <- F  # T when testing code, F otherwise

# Range of Prediction Horizons -----
search_grid <- tibble(
  name = paste0("m", 1:12),
  horizon = as.list(1:12)
)
# Read configuration 
if (TRUE) {
  # Prepare for slurm to submit configuration files flexibly
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- args[1]  # Get the first argument, which is the config file name
  
  config_params <- read_config(config_file)
} else {
  config_params <- list(
    "horizon" = 1
  )
}
search_grid <- search_grid |> filter(name %in% paste0("m", config_params$horizon))
# Create output folder if missing 
output_path <- paste0("Data/Generated/Models/", format(Sys.time(), "%Y%m%d"), "-", settings$screens$size_screen)
if (!dir.exists(output_path)) {
  # Create relevant directories
  dir.create(output_path) # Overall
  # Save Main.R and settings file to new directory
  file.copy("Main.R", paste0(output_path, "/Main.R"))
  file.copy("slurm_fit_models.R", paste0(output_path, "/slurm_fit_models.R"))
  settings |> saveRDS(paste0(output_path, "/settings.RDS"))
}
tictoc::tic("Total run time")
source("1 - Prepare Data.R", echo = T)
source("2 - Fit Models.R", echo = T)
