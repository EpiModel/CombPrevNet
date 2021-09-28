# This pulls in the default `param` object and the trackers
source("R/utils-inputs.R", local = TRUE)
# pkgload::load_all("../EpiModelHIV-p")
orig <- readRDS("out/est/restart.rds")

nsteps <- 52 * 20

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

sim <- netsim(orig, param, init, control)

library(tidyverse)
d <- as_tibble(sim)
