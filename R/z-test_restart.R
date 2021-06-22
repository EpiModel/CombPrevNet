# This pulls in the default `param` object and the trackers
source("R/utils-params.R", local = TRUE)
# pkgload::load_all("../EpiModelHIV-p")
orig <- readRDS("out/est/restart.rds")

nsteps <- 52 * 20

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 4,
  ncores = 4,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

param$param_updaters <- c(
  param$param_updaters, # you need to keep the default updaters
  # the new updater(s), that will change the scenario at timestep == at
  list(
    at = 70 * 52 + 1,
    param = list(
      part.index.prob = 1,
      part.ident.main.prob = 1,
      part.ident.casl.prob = 1,
      part.ident.ooff.prob = 1,
      # Part Serv Params
      part.hiv.test.rate   = rep(1, 3),
      part.prep.start.prob = rep(0, 3),
      part.tx.init.prob    = rep(0, 3),
      part.tx.reinit.prob  = rep(0, 3)
    )
  )
)

sim <- netsim(orig, param, init, control)
