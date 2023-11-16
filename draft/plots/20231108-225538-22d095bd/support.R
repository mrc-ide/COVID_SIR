# Function to plot parameter correlations
plot_parameter_correlation_ggplot <- function(stoch_sir_fit, det_sir_fit, det_adap_sir_fit) {  
  # Combine the data into a single data frame
  df_stoch <- data.frame(beta = stoch_sir_fit$pars[, 1], gamma = stoch_sir_fit$pars[, 2], Type = "Stochastic")
  df_det <- data.frame(beta = det_sir_fit$pars[, 1], gamma = det_sir_fit$pars[, 2], Type = "Deterministic")
  df_adap <- data.frame(beta = det_adap_sir_fit$pars[, 1], gamma = det_adap_sir_fit$pars[, 2], Type = "Adaptive")
  df <- rbind(df_stoch, df_det, df_adap)
  
  # Plot using ggplot2
  ggplot(df, aes(x = beta, y = gamma, color = Type)) +
    geom_point(alpha = 0.5) + # Translucent points
    scale_color_manual(values = c("Stochastic" = "#ff0000", "Deterministic" = "#0000ff", "Adaptive" = "#d000ff")) +
    theme_minimal() +
    labs(color = "Model Type") +
    ggtitle("Parameter Correlation Plot") +
    xlab(expression(beta)) + # Symbolic x-axis label
    ylab(expression(gamma)) + # Symbolic y-axis label
    xlim(0.15, 0.35) +
    ylim(0.05, 0.2)
}

# Function to create a density data frame for heatmap
create_density_df <- function(df, x_bins, y_bins) {
  df %>% 
    mutate(
      x_bin = cut_width(beta, width = x_bins, boundary = 0),
      y_bin = cut_width(gamma, width = y_bins, boundary = 0)
    ) %>% 
    group_by(x_bin, y_bin, Type) %>% 
    summarise(count = n(), .groups = 'drop')
}

# Function to plot combined parameter correlation heatmap
plot_combined_parameter_correlation_heatmap <- function(stoch_sir_fit, det_sir_fit, det_adap_sir_fit) {
  # Combine the data into a single data frame
  df_stoch <- data.frame(beta = stoch_sir_fit$pars[, 1], gamma = stoch_sir_fit$pars[, 2], Type = "Stochastic")
  df_det <- data.frame(beta = det_sir_fit$pars[, 1], gamma = det_sir_fit$pars[, 2], Type = "Deterministic")
  df_adap <- data.frame(beta = det_adap_sir_fit$pars[, 1], gamma = det_adap_sir_fit$pars[, 2], Type = "Adaptive")
  df <- rbind(df_stoch, df_det, df_adap)

  # Plot using ggplot2 with density heatmap
  ggplot(df, aes(x = beta, y = gamma, fill = ..density..)) +
    stat_density_2d(geom = "raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "blue") +
    facet_wrap(~ Type) +
    theme_minimal() +
    labs(fill = "Density") +
    ggtitle("Combined Parameter Space Exploration Heatmap") +
    xlab(expression(beta)) +
    ylab(expression(gamma)) +
    xlim(0.15, 0.35) +
    ylim(0.05, 0.2)
}

# Function to process simulation results for state plot
process_sim_results <- function(sim_data, name_prefix) {
  res <- sim_data$trajectories$state[2, ,-1L]
  y <- cbind(colMeans(res),
             t(apply(res, 2, quantile, probs = c(0.025, 0.975))))
  colnames(y) <- c(paste0(name_prefix, "_mean"),
                   paste0(name_prefix, "_lb"),
                   paste0(name_prefix, "_ub"))
  return(y)
}

# Function to plot SIR states
plot_sir_model <- function(det_sir_fit, det_adap_sir_fit, stoch_sir_fit, true_history, incidence) {
  # Process simulation results
  y1 <- process_sim_results(det_sir_fit, "det")
  y2 <- process_sim_results(det_adap_sir_fit, "adap")
  y3 <- process_sim_results(stoch_sir_fit, "stoch")
  
  # Add true history data to the stochastic results
  y3 <- cbind(y3, data = true_history[2, ,-1L])
  
  # Combine all results
  y <- cbind(y1, y2, y3)
  y <- as.data.frame(y)
  
  # Prepare date for x-axis
  date <- true_history[1,,1:100]
  
  # Define colors for the plot
  fit_cols <- setNames(c("purple", "#00ff519c", "blue", "red"), 
                       c("det_mean", "adap_mean", "stoch_mean", "data"))

  # Create the plot
  ggplot(y, aes(x = date)) +
    geom_line(aes(y = det_mean), color = fit_cols["det_mean"], size = 1) +
    geom_ribbon(aes(ymin = det_lb, ymax = det_ub), fill = fit_cols["det_mean"], alpha = 0.25) +
    geom_line(aes(y = adap_mean), color = fit_cols["adap_mean"], size = 1) +
    geom_ribbon(aes(ymin = adap_lb, ymax = adap_ub), fill = fit_cols["adap_mean"], alpha = 0.25) +
    geom_line(aes(y = stoch_mean), color = fit_cols["stoch_mean"], size = 1) +
    geom_ribbon(aes(ymin = stoch_lb, ymax = stoch_ub), fill = fit_cols["stoch_mean"], alpha = 0.25) +
    geom_point(aes(y = data), color = fit_cols["data"], size = 1.5) +
    scale_color_manual(values = fit_cols) +
    labs(y = "Daily infections", x = "Time (Days)") +
    theme_minimal() +
    theme(axis.line = element_line(),
          legend.text = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12)) +
              guides(color = guide_legend(title = "Legend"), fill = guide_legend(title = "Legend"))
}

# Function to create particle filter scatter plot with boxplot for publication
create_particle_filter_plot <- function(filter_data) {
  # Prepare data
  colnames(filter_data$deterministic_filtered) <- filter_data$n_particles
  df_det <- as.data.frame(filter_data$deterministic_filtered) %>%
    mutate(variable = "MLP_deterministic_sample") %>%
    pivot_longer(!variable, names_to = "n_particles")

  colnames(filter_data$stochastic_filtered) <- filter_data$n_particles
  df_stoch <- as.data.frame(filter_data$stochastic_filtered) %>%
    mutate(variable = "MLP_stochastic_sample") %>%
    pivot_longer(!variable, names_to = "n_particles")

  df_combined <- rbind(df_det, df_stoch) %>%
    mutate(n_particles = as.numeric(n_particles), n_particles = factor(n_particles))

  # Plot
  ggplot(df_combined, aes(y = value, x = n_particles, color = variable, fill = variable)) +
    geom_point(position = position_jitterdodge(jitter.width = .25, dodge.width = 0.6), size = 1, pch = 19) +
    stat_boxplot(geom = "errorbar", width = .25, position = position_dodge(0.6), color = "#00000055") +
    geom_boxplot(width = .25, position = position_dodge(0.6), outlier.shape = NA, alpha = 0.5, color = "black") +
    scale_color_manual(values = c("MLP_deterministic_sample" = "#0000ff55", "MLP_stochastic_sample" = "#ff000055")) +
    scale_fill_manual(values = c("MLP_deterministic_sample" = "#0000ff55", "MLP_stochastic_sample" = "#ff000055")) +
    theme_classic() +
    theme(legend.position = c(.8, .1), panel.grid.major = element_line(), panel.grid.minor = element_line()) +
    labs(fill = "Fitted pipeline", color = "Fitted pipeline", title = "Particle-Filter Attributable Stochasticity") +
    ylab("MLP")
}