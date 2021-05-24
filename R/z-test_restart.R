lnt <- TRUE
source("R/utils-params.R", local = TRUE)
orig <- readRDS("out/restart_test.rds")

window_size <- 52

nsteps <- 52 * 2

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


# debug(hivtest_msm)
sim <- netsim(orig, param, init, control)
