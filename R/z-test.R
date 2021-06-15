lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)

orig <- readRDS("out/est/restart.rds")

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 80 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
  nsims = 28,
  ncores = 28,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE
)

# Scenarios --------------------------------------------------------------------
# requires <list variables>
source("R/utils-scenarios.R")
