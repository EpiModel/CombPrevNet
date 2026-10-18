lnt <- TRUE
source("R/utils-params.R", local = TRUE)
orig <- readRDS("out/est/restart.rds")

window_size <- 52

prep_start_time <- 52 * 61 + 1
test_params <- list(
  prep.start = prep_start_time,
  riskh.start = prep_start_time - 52,
  part.ident.start = 60 * 52 + 1,
  # Part ident parameters
  part.index.window = 0,
  part.index.degree = 1,
  part.index.prob = 1,
  part.ident.main.window = window_size,
  part.ident.casl.window = window_size,
  part.ident.ooff.window = window_size,
  truncate.plist = 100,
  # see "R/z-indent_prob_calib.R"
  part.ident.main.prob = 1,
  part.ident.casl.prob = 1,
  part.ident.ooff.prob = 1,

  part.hiv.test.rate = rep(1, 3),
  part.prep.start.prob = rep(1, 3),
  part.tx.init.prob = rep(1, 3),
  part.tx.reinit.prob = rep(1, 3)
)

param <- update_params(param, test_params)

nsteps <- 100

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
