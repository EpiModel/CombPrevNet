#
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"
#
source("R/utils-inputs.R", local = TRUE)

nsim <- 6

control <- control_msm(
  nsteps = 52 * 20,
  nsims = nsim,
  ncores = nsim,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  truncate.el.cuml = 53,
  tracker.list = epi_trackers,
  raw.output = FALSE,

  initialize.FUN    = initialize_msm,
  updater.FUN       = function(dat, at) dat, #updater.net,
  aging.FUN         = aging_msm,
  departure.FUN     = departure_msm,
  arrival.FUN       = arrival_msm,
  partident.FUN     = partident_msm,
  hivtest.FUN       = hivtest_msm,
  hivtx.FUN         = hivtx_msm,
  hivprogress.FUN   = hivprogress_msm,
  hivvl.FUN         = hivvl_msm,
  resim_nets.FUN    = simnet_msm,
  acts.FUN          = acts_msm,
  condoms.FUN       = condoms_msm,
  position.FUN      = position_msm,
  prep.FUN          = prep_msm,
  hivtrans.FUN      = hivtrans_msm,
  stitrans.FUN      = stitrans_msm,
  stirecov.FUN      = stirecov_msm,
  stitx.FUN         = stitx_msm,
  prev.FUN          = prevalence_msm,
  trackers.FUN      = trackers.net,
  verbose.FUN       = verbose.net,

  verbose = TRUE
)

# debug(updateModelTermInputs)
sim <- netsim(orig, param, init, control)

d <- as.data.frame(sim)
