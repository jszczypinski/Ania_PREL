# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("readxl", "data.table", "lme4", "afex", "DHARMa", "flextable"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source("./R/functions.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(file, "./data/Data_mixedAnova.xlsx", format = "file"),
  tar_target(data_L1, get_data(file, "polish")),
  tar_target(data_L2, get_data(file, "PJM")),
  tar_target(model_L1, fit_poisson_model(data_L1)),
  tar_target(model_L2, fit_poisson_model(data_L2)),
  tar_target(diag_plot_L1, plot_model_diagnostics(model_L1)),
  tar_target(diag_plot_L2, plot_model_diagnostics(model_L2)),
  tar_target(pvals_L1, get_pvalues(model_L1, data_L1, .nsim = 10000)),
  tar_target(pvals_L2, get_pvalues(model_L2, data_L2, .nsim = 10000)),
  tar_target(table_L1, make_table(pvals_L1, "polish")),
  tar_target(table_L2, make_table(pvals_L2, "PJM"))
)
