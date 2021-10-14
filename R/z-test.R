source("R/utils-inputs.R", local = TRUE)

control <- control_msm(
  nsteps = 52 * 65,
  nsims = 1,
  ncores = 1,
  truncate.el.cuml = 53,
  tracker.list = epi_trackers,

  initialize.FUN = initialize_msm,
  param_updater.FUN = function(dat, at) dat,
  aging.FUN = function(dat, at) dat,
  departure.FUN = departure_msm,
  arrival.FUN = arrival_msm,
  partident.FUN = function(dat, at) dat,
  hivtest.FUN = function(dat, at) dat,
  hivtx.FUN = function(dat, at) dat,
  hivprogress.FUN = function(dat, at) dat,
  hivvl.FUN = function(dat, at) dat,
  resim_nets.FUN = simnet_msm,
  acts.FUN = function(dat, at) dat,
  condoms.FUN = function(dat, at) dat,
  position.FUN = function(dat, at) dat,
  prep.FUN = function(dat, at) dat,
  hivtrans.FUN = function(dat, at) dat,
  stitrans.FUN = function(dat, at) dat,
  stirecov.FUN = function(dat, at) dat,
  stitx.FUN = function(dat, at) dat,
  trackers.FUN = function(dat, at) dat,
  prev.FUN = prevalence_msm,
  verbose.FUN = verbose.net,

  verbose = FALSE,
  raw.output = FALSE
)

options(browser = "firefox")

profvis::profvis({
  sim <- netsim(orig, param, init, control)
})
