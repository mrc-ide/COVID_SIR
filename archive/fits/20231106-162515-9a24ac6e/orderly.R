# setwd("Z:/Yasin/COVID_SIR/src/fits")

lapply(c("global_util.R", "support.R", "plot.R"), source)

orderly2::orderly_parameters(short_run = NULL,
                            deterministic = NULL,
                            adaptive_proposal = FALSE)

orderly2::orderly_dependency("data", "latest()",
                            c("data.rds" = "outputs/data.rds",
                            "model.rds" = "outputs/model.rds"))

print(c(short_run, deterministic, adaptive_proposal))

if (!deterministic && adaptive_proposal) {
    stop("adaptive pMCMC method not yet implemented, please ensure deterministic = TRUE if adaptive_proposal != NULL")
}

sir <- readRDS("model.rds")
if(!is.null(sir)) print("success!")

incidence <- readRDS("data.rds")
if(!is.null(incidence)) print("success!")

dt <- 0.25
data <- mcstate::particle_filter_data(incidence,
  initial_time = 0,
  time = "day",
  rate = 1 / dt)

proposal_kernel <- rbind(c(0.00057, 0.00034), c(0.00034, 0.00026))

pars <- mcstate::pmcmc_parameters$new(
  list(mcstate::pmcmc_parameter("beta", 0.2, min = 0, max = 1,
                                prior = function(p) log(1e-10)),
       mcstate::pmcmc_parameter("gamma", 0.1, min = 0, max = 1,
                                prior = function(p) log(1e-10))),
  proposal = proposal_kernel)

if (short_run) {
  burnin <- 1
  n_steps <- 20
  n_sample <- 10
  n_chains <- 4
} else {
  burnin <- 500
  n_steps <- 1500
  n_sample <- 1000
  n_chains <- 4
}
n_steps_retain <- ceiling(n_sample / n_chains)

n_threads_total <- spimalot:::spim_control_cores()
if (deterministic) {
  n_workers <- min(n_chains, n_threads_total)
  n_threads <- n_threads_total / n_workers
  p <- mcstate::particle_deterministic$new(data, sir, compare = NULL, index = index,
                                            n_threads = n_threads)
} else {
  max_workers <- 4
  pos <- seq_len(max_workers)
  n_workers <- max(pos[n_threads_total %% pos == 0 & pos <= n_chains])
  n_threads <- n_threads_total / n_workers
  
  n_particles <- 192
  p <- mcstate::particle_filter$new(data, sir, n_particles = n_particles,
                                    compare = NULL, index = index,
                                    n_threads = n_threads)
}
n_threads <- n_threads_total / n_workers

control <- 
  mcstate::pmcmc_control(n_steps = n_steps, n_burnin = burnin,
                         n_threads_total = n_threads_total,
                         n_workers = n_workers, n_chains = n_chains,
                         n_steps_retain = n_steps_retain, save_state = TRUE,
                         adaptive_proposal = adaptive_proposal,
                         save_trajectories = TRUE, progress = TRUE)

fit <-  mcstate::pmcmc(pars, p, control = control)

dir.create("outputs", FALSE, TRUE)
saveRDS(fit, "outputs/fit.rds")
saveRDS(pars, "outputs/pars.rds")

dir.create("figs", FALSE, TRUE)
write_png("figs/traceplots.png", width = 3000, height = 1800, res = 200,
          plot_traceplots(fit))
