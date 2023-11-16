##### Particle filter rewrite
## The purpose of this task is to use the fitted sample parameters
## from deterministic and stochastic fits and run them through a 
## stochatic particle filter of varying sizes in order to re-calculate the
## log-posterior and likelihood
## TDOD
## overall the task is largeley complete though it will benefit from refactoring
## some awfully named parameters
## perhaps porting over some functionality over to the support script

## -----------------------------------------------------------------------------
## Part 5 Toy model particle filter test
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Load SIR fit and parameters
## -----------------------------------------------------------------------------
#setwd("Z:/Yasin/COVID_SIR/src/filter_test")
lapply(c("global_util.R", "support.R"), source)

orderly2::orderly_parameters(short_run = TRUE)

# Define a function to load dependencies based on given parameters
load_dependency <- function(short_run, deterministic, adaptive_proposal, prefix) {
  query <- sprintf("latest(parameter:short_run == %s && parameter:deterministic == %s && parameter:adaptive_proposal == %s)",
                   toupper(as.character(short_run)),
                   toupper(as.character(deterministic)),
                   toupper(as.character(adaptive_proposal)))
  files <- setNames(
  c("outputs/fit.rds", "outputs/pars.rds"),
  c(sprintf("%s_fit.rds", prefix), sprintf("%s_pars.rds", prefix))
)
  orderly2::orderly_dependency("fits", query, files)
}

# Load dependencies
load_dependency(FALSE, TRUE, FALSE, "deterministic")
load_dependency(FALSE, TRUE, TRUE, "deterministic_adaptive")
load_dependency(FALSE, FALSE, FALSE, "stochastic")

# Read the RDS files into the environment
sir_fits_filenames <- c("deterministic_adaptive_fit", "stochastic_fit")
sir_pars_filenames <- c("deterministic_adaptive_pars", "stochastic_pars")

# Append '.rds' to each filename and read the files
sir_fits <- lapply(paste0(sir_fits_filenames, ".rds"), readRDS)
sir_pars <- lapply(paste0(sir_pars_filenames, ".rds"), readRDS)

det_adap_sir_fit <- sir_fits[[1]]
stoch_sir_fit <- sir_fits[[2]]

det_adap_sir_pars <- sir_pars[[1]]
stoch_sir_pars <- sir_pars[[2]]

det_adap_pars <- det_adap_sir_fit$pars_full
stoch_pars <- stoch_sir_fit$pars_full

if (short_run) {
  n_filter_iter <- 10
} else {
  n_filter_iter <- 1000
}

dir.create("outputs", FALSE, TRUE)

## -----------------------------------------------------------------------------
## Part 6 Varying single parameter (sample) value across fixed particle filter size
## -----------------------------------------------------------------------------

## Build stochastic filter
set.seed(1234)

data <- stoch_sir_fit$predict$filter$data
sir <- stoch_sir_fit$predict$filter$model
index <- stoch_sir_fit$predict$filter$index
n_particles <- 192

n_threads <- spimalot::spim_control_cores()

filter_stochastic <-
  mcstate::particle_filter$new(data, sir, n_particles = n_particles,
                               compare = NULL, index = index, seed = 1L,
                               n_threads = n_threads)

#sample_pars_index <- sample(nrow(stoch_pars), 1)
sample_pars_index <- which.max(stoch_pars[, "gamma"])
## parameters to index and test 
det_adap_pars <- det_adap_pars[sample_pars_index, ]
stoch_pars <- stoch_pars[sample_pars_index, ]
param_test <- "beta"

## sample values near true value (deterministic and stochastic outputs)
## number of values to try
n_try <- 50

## parameter values at either side of the fitted parameter value
param_values <- seq(from = 0,
                    to = det_adap_pars[param_test] * 3,
                    length.out = n_try)

## run filter on single parameter
## (sample around "true" deterministic parameter value)
## calculate the log-posterior (likelihood + prior)
det_filtered_samples <- vapply(seq_along(param_values),
  function(i) {
    det_adap_pars[param_test] <- param_values[i]
    filter_stochastic$run(stoch_sir_pars$model(det_adap_pars)) +
      stoch_sir_pars$prior(det_adap_pars)
  },
  numeric(1)
)



## run filter on single parameter
## (sample around "true" stochastic parameter value)
## calculate the log-posterior (likelihood + prior)
stoch_filtered_sample <- vapply(seq_along(param_values),
  function(i) {
    stoch_pars[param_test] <- param_values[i]
    filter_stochastic$run(stoch_sir_pars$model(stoch_pars)) + 
      stoch_sir_pars$prior(stoch_pars)
  },
  numeric(1)
)

stoch_filtered_sample <- unlist(stoch_filtered_sample)

## save filter outputs
filter_samples <- list(det_filtered_samples, stoch_filtered_sample)
names(filter_samples) <- c("det_filtered_samples", "stoch_filtered_sample")
saveRDS(filter_samples, "outputs/filter_samples.rds")

## -----------------------------------------------------------------------------
## Part 7 Varying particle filter size across parameter sample
## -----------------------------------------------------------------------------

## define the number of particle sizes to test
## the number of particle filter iterations
det_adap_pars <- det_adap_sir_fit$pars
stoch_pars <- stoch_sir_fit$pars
n_particles <- 2 ^ c(6:10)
filter_data <- list(NULL)

run_n_particles <- function(n, pars) {
  filter <- mcstate::particle_filter$new(data, sir,
                                         n_particles = n,
                                         compare = compare, index = index,
                                         seed = 1L, n_threads = n_threads)
  
  vapply(seq_len(nrow(pars)),
         function(i) {
           filter$run(
             stoch_sir_pars$model(
               pars[i, ]
             )
           ) + stoch_sir_pars$prior(pars[i, ])
         },
         numeric(1))
}

deterministic_filtered <-
  vapply(n_particles, function (i) run_n_particles(i, det_adap_pars),
         numeric(nrow(det_adap_pars)))

stochastic_filtered <-
  vapply(n_particles, function (i) run_n_particles(i, stoch_pars),
         numeric(nrow(stoch_pars)))

filter_data <- list(deterministic_filtered = deterministic_filtered,
                    stochastic_filtered = stochastic_filtered,
                    n_particles = n_particles)

saveRDS(filter_data, "outputs/filter_data.rds")
