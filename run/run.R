path <- setwd("Z:/Yasin/COVID_SIR/src/")

## Create SIR model and SIR data simulation
orderly2::orderly_run("data", root = path)

## Fit SIR model
runs <- list(
  list(short_run = FALSE, deterministic = TRUE, adaptive_proposal = FALSE),
  list(short_run = FALSE, deterministic = TRUE, adaptive_proposal = TRUE),
  list(short_run = FALSE, deterministic = FALSE, adaptive_proposal = FALSE)
)

lapply(runs, function(params) {
  orderly2::orderly_run("fits", params, root = path)
})

## Run particle filter on SIR samples
orderly2::orderly_run("filter_test", 
                        list(short_run = TRUE),
                        root = path)

## Create plots from SIR fits and particle filter samples
orderly2::orderly_run("plots", root = path)
