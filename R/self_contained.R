# pkgload::load_all("../EMHIVP2")
library(EpiModelHIV) # "EpiModel/EpiModelHIV-p@CPN_new_APIs"
library(ARTnet)

epistats <- build_epistats(
  geog.lvl = "city",
  geog.cat = "Atlanta",
  init.hiv.prev = c( 0.33, 0.137, 0.084),
  race = TRUE
)

netparams <- build_netparams(epistats = epistats, smooth.main.dur = TRUE)
netstats <- build_netstats(
  epistats,
  netparams,
  expect.mort = 0.000478213,
  network.size = 10000
)

# 0. Initialize Network --------------------------------------------------------

num <- netstats$demog$num
nw <- network::network.initialize(num, directed = FALSE)

attr.names <- names(netstats$attr)
attr.values <- netstats$attr
nw <- network::set.vertex.attribute(nw, attr.names, attr.values)
nw_main <- nw
nw_casl <- nw
nw_inst <- nw

# 1. Main Model ----------------------------------------------------------------

# Formula
model_main <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -1) +
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1) +
  nodefactor("deg.casl", levels = -1) +
  concurrent +
  degrange(from = 3) +
  nodematch("role.class", diff = TRUE, levels = 1:2)

# Target Stats
netstats_main <- c(
  edges                = netstats$main$edges,
  nodematch_age.grp    = netstats$main$nodematch_age.grp,
  nodefactor_age.grp   = netstats$main$nodefactor_age.grp[-1],
  nodematch_race       = netstats$main$nodematch_race_diffF,
  nodefactor_race      = netstats$main$nodefactor_race[-1],
  nodefactor_deg.casl  = netstats$main$nodefactor_deg.casl[-1],
  concurrent           = netstats$main$concurrent,
  degrange             = 0,
  nodematch_role.class = c(0, 0)
)
netstats_main <- unname(netstats_main)

# Fit model
fit_main <- netest(
  nw_main,
  formation = model_main,
  target.stats = netstats_main,
  coef.diss = netstats$main$diss.byage,
  set.control.ergm = control.ergm(
    MCMLE.maxit = 500,
    SAN.maxit = 3,
    SAN.nsteps.times = 3,
    MCMC.samplesize = 10000,
    MCMC.interval = 5000),
  verbose = TRUE
)

# 2. Casual Model ---------------------------------------------------------

# Formula
model_casl <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = c(-1, -5)) +
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1) +
  nodefactor("deg.main", levels = -3) +
  concurrent +
  degrange(from = 4) +
  nodematch("role.class", diff = TRUE, levels = 1:2)

# Target Stats
netstats_casl <- c(
  edges                = netstats$casl$edges,
  nodematch_age.grp    = netstats$casl$nodematch_age.grp,
  nodefactor_age.grp   = netstats$casl$nodefactor_age.grp[-c(1, 5)],
  nodematch_race       = netstats$casl$nodematch_race_diffF,
  nodefactor_race      = netstats$casl$nodefactor_race[-1],
  nodefactor_deg.main  = netstats$casl$nodefactor_deg.main[-3],
  concurrent           = netstats$casl$concurrent,
  degrange             = 0,
  nodematch_role.class = c(0, 0)
)
netstats_casl <- unname(netstats_casl)

# Fit model
fit_casl <- netest(
  nw_casl,
  formation = model_casl,
  target.stats = netstats_casl,
  coef.diss = netstats$casl$diss.byage,
  set.control.ergm = control.ergm(
    MCMLE.maxit = 500,
    SAN.maxit = 3,
    SAN.nsteps.times = 3),
  verbose = FALSE
)

# 3. One-Off Model --------------------------------------------------------

# Formula
model_inst <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", levels = -1) +
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1) +
  nodefactor("risk.grp", levels = -5) +
  nodefactor("deg.tot", levels = -1) +
  nodematch("role.class", diff = TRUE, levels = 1:2)

# Target Stats
netstats_inst <- c(
  edges                = netstats$inst$edges,
  nodematch_age.grp    = sum(netstats$inst$nodematch_age.grp),
  nodefactor_age.grp   = netstats$inst$nodefactor_age.grp[-1],
  nodematch_race       = netstats$inst$nodematch_race_diffF,
  nodefactor_race      = netstats$inst$nodefactor_race[-1],
  nodefactor_risk.grp  = netstats$inst$nodefactor_risk.grp[-5],
  nodefactor_deg.tot   = netstats$inst$nodefactor_deg.tot[-1],
  nodematch_role.class = c(0, 0)
)
netstats_inst <- unname(netstats_inst)

# Fit model
fit_inst <- netest(
  nw_inst,
  formation = model_inst,
  target.stats = netstats_inst,
  coef.diss = dissolution_coefs(~offset(edges), 1),
  set.control.ergm = control.ergm(
    MCMLE.maxit = 500,
    SAN.maxit = 3,
    SAN.nsteps.times = 3),
  verbose = FALSE
)

# 4. Save Data ------------------------------------------------------------
out <- list(fit_main, fit_casl, fit_inst)
# saveRDS(out, file = "out/est/netest.rds")
# saveRDS(netstats, file = "out/est/netstats.rds")
# saveRDS(epistats, file = "out/est/epistats.rds")
orig <- out

# 5. Model config --------------------------------------------------------------
# orig <- readRDS("out/est/netest.rds")
# netstats <- readRDS("out/est/netstats.rds")
# epistats <- readRDS("out/est/epistats.rds")

# the `simnet_msm` function used here
simnet_msm <- function(dat, at) {

  ## Parameters
  truncate.plist <- get_param(dat, "truncate.plist")
  truncate.el.cuml <- get_control(dat, "truncate.el.cuml")

  ## Edges correction
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

  plist1 <- update_plist(dat, at, ptype = 1)


  ## Casual network
  nwparam <- EpiModel::get_nwparam(dat, network = 2)

  dat <- set_attr(dat, "deg.main", EpiModel::get_degree(dat$el[[1]]))
  dat <- tergmLite::updateModelTermInputs(dat, network = 2)

  # rv <- tergmLite::simulate_network(
  #   state = dat$p[[2]]$state,
  #   coef = c(nwparam$coef.form, nwparam$coef.diss$coef.adj),
  #   control = dat$control$mcmc.control[[2]],
  #   save.changes = TRUE
  # )

  # dat$el[[2]] <- rv$el

  # plist2 <- update_plist(dat, at, ptype = 2)

  # ## One-off network
  # nwparam <- EpiModel::get_nwparam(dat, network = 3)

  # dat <- set_attr(dat, "deg.tot",
  #   pmin(get_attr(dat, "deg.main") + EpiModel::get_degree(dat$el[[2]]), 3))
  # dat <- tergmLite::updateModelTermInputs(dat, network = 3)

  # rv <- tergmLite::simulate_ergm(state = dat$p[[3]]$state,
  #   coef = nwparam$coef.form,
  #   control = dat$control$mcmc.control[[3]]
  # )

  # dat$el[[3]] <- rv$el

  # plist3 <- update_plist(dat, at, ptype = 3)

  # dat$temp$plist <- rbind(plist1, plist2, plist3)
  # if (truncate.plist != Inf) {
  #   to.keep.stop <- at - dat$temp$plist[, "stop"]
  #   to.keep <- which(is.na(dat$temp$plist[, "stop"]) | to.keep.stop < truncate.plist)
  #   dat$temp$plist <- dat$temp$plist[to.keep, , drop = FALSE]
  # }

  # if (get_control(dat, "save.nwstats") == TRUE) {
  #   dat <- calc_nwstats(dat, at)
  # }

  # for (n_network in seq_len(3)) {
  #   dat <- update_cumulative_edgelist(dat, at, n_network, truncate.el.cuml)
  # }

  return(dat)
}

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  truncate.plist = 100
)

init <- init_msm(
  prev.ugc = 0.05,
  prev.rct = 0.05,
  prev.rgc = 0.05,
  prev.uct = 0.05
)

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
  resim_nets.FUN    = simnet_msm, # see above
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

# Model Run --------------------------------------------------------------------
sim <- netsim(orig, param, init, control)

