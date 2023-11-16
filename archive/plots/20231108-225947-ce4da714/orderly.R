library(dplyr)
library(tidyr)
library(ggplot2)

lapply(c("global_util.R", "support.R"), source)

orderly2::orderly_dependency("filter_test", "latest()",
                            c("deterministic_fit.rds" = "deterministic_fit.rds",
                              "deterministic_adaptive_fit.rds" = "deterministic_adaptive_fit.rds",
                              "stochastic_fit.rds" = "stochastic_fit.rds",
                              "deterministic_pars.rds" = "deterministic_pars.rds",
                              "deterministic_adaptive_pars.rds" = "deterministic_adaptive_pars.rds",
                              "stochastic_pars.rds" = "stochastic_pars.rds",
                              "filter_data.rds" = "outputs/filter_data.rds",
                            "filter_samples.rds" = "outputs/filter_samples.rds"))

orderly2::orderly_dependency("data", "latest()",
                            c("sir_true_history.rds" = "outputs/true_history.rds"))

det_sir_fit <- readRDS("deterministic_fit.rds")
det_adap_sir_fit <- readRDS("deterministic_adaptive_fit.rds")
stoch_sir_fit <- readRDS("stochastic_fit.rds")

det_sir_pars <- readRDS("deterministic_pars.rds")
det_adap_sir_pars <- readRDS("deterministic_adaptive_pars.rds")
stoch_sir_pars <- readRDS("stochastic_pars.rds")

filter_data <- readRDS("filter_data.rds")
filter_samples <- readRDS("filter_samples.rds")

true_history <- readRDS("sir_true_history.rds")

incidence <- true_history["cases_inc", 1, ]

# plot_parameter_correlation_ggplot(stoch_sir_fit, det_sir_fit, det_adap_sir_fit)

# plot_combined_parameter_correlation_heatmap(stoch_sir_fit, det_sir_fit, det_adap_sir_fit)

# plot_sir_model(det_sir_fit, det_adap_sir_fit, stoch_sir_fit, true_history, incidence)

# create_particle_filter_plot(filter_data)

# Create directory for figures
dir.create("figs", showWarnings = FALSE)

# List of plotting functions and their arguments
plot_functions <- list(
  plot_parameter_correlation_ggplot = list(stoch_sir_fit, det_sir_fit, det_adap_sir_fit),
  plot_combined_parameter_correlation_heatmap = list(stoch_sir_fit, det_sir_fit, det_adap_sir_fit),
  plot_sir_model = list(det_sir_fit, det_adap_sir_fit, stoch_sir_fit, true_history, incidence),
  create_particle_filter_plot = list(filter_data)
)

# Generate and save each plot
lapply(names(plot_functions), function(func_name) {
  file_name <- paste0("figs/", func_name, ".png")
  generate_and_save_plots(func_name, plot_functions[[func_name]], file_name)
})