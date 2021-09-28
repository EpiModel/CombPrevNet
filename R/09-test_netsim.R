
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"
#
source("R/utils-inputs.R", local = TRUE)

nsim <- 1
control <- control_msm(
  nsteps = 52 * 60,
  nsims = nsim,
  ncores = nsim,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  truncate.el.cuml = 53,
  tracker.list = epi_trackers,
  raw.output = FALSE,
  verbose = TRUE
)

# options(error = recover)
# debug(tergm_MCMC_slave)
sim <- netsim(orig, param, init, control)

# sim$epi <- sim$epi["num"] # keep only the "num" epi tracker
# saveRDS(sim, "out/est/restart.rds")

