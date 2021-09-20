#
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"
#
source("R/utils-inputs.R", local = TRUE)

control <- control_msm(
  nsteps = 52 * 1,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  tracker.list = epi_trackers,
  raw.output = TRUE,

  # truncate.el.cuml = 53,

  initialize.FUN    = initialize_msm,
  param_updater.FUN = NULL, # param_updater,
  aging.FUN         = aging_msm,
  departure.FUN     = departure_msm,
  arrival.FUN       = arrival_msm,
  partident.FUN     = NULL, # partident_msm,
  hivtest.FUN       = NULL, # hivtest_msm,
  hivtx.FUN         = NULL, # hivtx_msm,
  hivprogress.FUN   = NULL, # hivprogress_msm,
  hivvl.FUN         = NULL, # hivvl_msm,
  resim_nets.FUN    = simnet_msm,
  acts.FUN          = NULL, # acts_msm,
  condoms.FUN       = NULL, # condoms_msm,
  position.FUN      = NULL, # position_msm,
  prep.FUN          = NULL, # prep_msm,
  hivtrans.FUN      = NULL, # hivtrans_msm,
  stitrans.FUN      = NULL, # stitrans_msm,
  stirecov.FUN      = NULL, # stirecov_msm,
  stitx.FUN         = NULL, # stitx_msm,
  prev.FUN          = NULL, # prevalence_msm,
  verbose.FUN       = verbose.net,

  verbose = TRUE
)

# debug(initialize_msm)
sim <- netsim(orig, param, init, control)

# Error in simulate.ergm_model(m, nsim = nsim, seed = seed, coef = coef,  :
#   coef has 18 elements, while the model requires 16 parameters.
sim[[1]]$el_cuml

sim[[1]]$temp %>%
  as.data.frame() %>%
  dplyr::filter(
    plist.ptype %in% c(1, 2),
    plist.start > 1
  )

dat <- sim[[1]]

at <- 53
dat <- edges_correct_msm(dat, at)

## Main network
nwparam <- EpiModel::get_nwparam(dat, network = 1)

dat <- set_attr(dat, "deg.casl", EpiModel::get_degree(dat$el[[2]]))
dat <- tergmLite::updateModelTermInputs(dat, network = 1)

rv <- tergmLite::simulate_network(
  state = dat$p[[1]]$state,
  coef = c(nwparam$coef.form, nwparam$coef.diss$coef.adj),
  control = dat$control$mcmc.control[[1]],
  save.changes = TRUE
)

dat$el[[1]] <- rv$el

