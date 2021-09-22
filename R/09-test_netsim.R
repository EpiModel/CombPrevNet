#
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"
#
source("R/utils-inputs.R", local = TRUE)

param$truncate.plist <- 54
control <- control_msm(
  nsteps = 52 * 60,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  truncate.el.cuml = param$truncate.plist,
  tracker.list = list(), #epi_trackers,
  raw.output = FALSE,

  # truncate.el.cuml = 53,

  initialize.FUN    = initialize_msm,
  updater.FUN       = function(dat, at) dat, #updater.net,
  aging.FUN         = aging_msm,
  departure.FUN     = departure_msm,
  arrival.FUN       = arrival_msm,
  partident.FUN     = function(dat, at) dat, #partident_msm,
  hivtest.FUN       = function(dat, at) dat, #hivtest_msm,
  hivtx.FUN         = function(dat, at) dat, #hivtx_msm,
  hivprogress.FUN   = function(dat, at) dat, #hivprogress_msm,
  hivvl.FUN         = function(dat, at) dat, #hivvl_msm,
  resim_nets.FUN    = simnet_msm,
  acts.FUN          = function(dat, at) dat, #acts_msm,
  condoms.FUN       = function(dat, at) dat, #condoms_msm,
  position.FUN      = function(dat, at) dat, #position_msm,
  prep.FUN          = function(dat, at) dat, #prep_msm,
  hivtrans.FUN      = function(dat, at) dat, #hivtrans_msm,
  stitrans.FUN      = function(dat, at) dat, #stitrans_msm,
  stirecov.FUN      = function(dat, at) dat, #stirecov_msm,
  stitx.FUN         = function(dat, at) dat, #stitx_msm,
  prev.FUN          = prevalence_msm,
  trackers.FUN      = function(dat, at) dat, #trackers.net,
  verbose.FUN       = verbose.net,

  verbose = FALSE
)

# debug(updateModelTermInputs)
sim <- netsim(orig, param, init, control)

# library(dplyr)

# d <- as_tibble(sim)

# d %>%
#   select(found_partners, found_partners2) %>%
#   summarise(across(everything(), ~ sum(.x, na.rm = T)))
