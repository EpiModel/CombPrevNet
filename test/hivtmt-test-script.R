# hivtx_msm debug script
# 

undebug(hivtx_msm)

param <- param_msm(epistats = epistats,
                   netstats = netstats,
                   hiv.scrn.rate = c(1, 1, 1),
                   part.lookback.main = 52,
                   part.lookback.casl = 24,
                   part.lookback.ooff = 6,
                   part.ident.main = 1,
                   part.ident.casl = 1,
                   part.ident.ooff = 1,
                   ptype.lookup = c(1, 2, 3),
                   part.prep.start.prob = 0.7,
                   prep.start = 1,
                   part.tx.init.prob = c(1, 1, 1),
                   part.tx.reinit.part.prob = c(1, 1, 1),
                   tx.halt.part.prob = c(1, 1, 1),
                   tx.halt.dur.rr = c(1, 1, 1),
                   tx.halt.full.rr = c(1, 1, 1),
                   riskh.start = TRUE
)

init <- init_msm()

control <- control_msm(simno = 1001,
                       start = 1,
                       nsteps = 200,
                       ncores = 1,
                       nsims = 1,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       tergmLite = TRUE)

set.seed(123)

dat <- initialize_msm(est, param, init, control, s = 1)

for (at in 2:400) {
  dat <- aging_msm(dat, at)
  dat <- departure_msm(dat, at)
  dat <- arrival_msm(dat, at)
  dat <- hivtest_msm(dat, at)
  dat <- hivtx_msm(dat, at)
  dat <- hivprogress_msm(dat, at)
  dat <- hivvl_msm(dat, at)
  dat <- simnet_msm(dat, at)
  dat <- acts_msm(dat, at)
  dat <- condoms_msm(dat, at)
  dat <- position_msm(dat, at)
  dat <- prep_msm(dat, at)
  dat <- hivtrans_msm(dat, at)
  dat <- stitrans_msm(dat, at)
  dat <- stirecov_msm(dat, at)
  dat <- stitx_msm(dat, at)
  dat <- prevalence_msm(dat, at)
  temp <- c(temp,
            length(which(dat$attr$cuml.time.on.tx > 0 & dat$attr$part.ident == at  & dat$attr$tx.status == 0)))
  verbose.net(dat, "progress", at = at)
}

at = at + 1
dat.temp <- dat

dat.temp <- aging_msm(dat.temp, at)
dat.temp <- departure_msm(dat.temp, at)
dat.temp <- arrival_msm(dat.temp, at)

debug(hivtx_msm)
dat.temp <- hivtx_msm(dat.temp, at)
