pkgload::load_all("../EMHIVP2") #library(EpiModelHIV)
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

control <- control_msm(
  nsteps = 52 * 1,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  raw.output = TRUE, # so `sim[[1]]` is the `dat` object unmodified

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

# Model Run --------------------------------------------------------------------
sim <- netsim(orig, param, init, control)

# Look at the Plists -----------------------------------------------------------
sim[[1]]$temp %>%
  as.data.frame() %>%
  dplyr::filter(
    plist.ptype %in% c(1, 2),
    plist.start > 1
  )

sim[[1]]$temp %>%
  as.data.frame() %>%
  dplyr::filter(
    plist.ptype %in% 3,
    plist.start > 1
  )

# Resimulate the casual network to see that nothing is happening ---------------
at <- control$nsteps
dat <- sim[[1]]

dat <- edges_correct_msm(dat, at)


nwparam <- EpiModel::get_nwparam(dat, network = 2)
dat <- tergmLite::updateModelTermInputs(dat, network = 2)

rv <- tergmLite::simulate_network(
  state = dat$p[[2]]$state,
  coef = c(nwparam$coef.form, nwparam$coef.diss$coef.adj),
  control = dat$control$mcmc.control[[2]],
  save.changes = TRUE
)

attr(rv$el, "changes")

