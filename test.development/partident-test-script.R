# hivtest_msm debug script
# 

suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")


param <- param_msm(epistats = epistats,
                   netstats = netstats,
                   part.hiv.test.rate  = c(1, 1, 1),
                   part.identification = 50,
                   part.lookback.main = 52,
                   part.lookback.casl = 52,
                   part.lookback.ooff = 52,
                   part.ident.main = 1,
                   part.ident.casl = 1,
                   part.ident.ooff = 1,
                   ptype.lookup = c(1, 2, 3),
                   prep.start = 101,
                   part.prep.start = 101
)

init <- init_msm()

control <- control_msm(simno = 1001,
                        nsteps =100,
                        ncores = 1,
                        nsims = 1,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE)

set.seed(123)

dat <- initialize_msm(est, param, init, control, s = 1)

for (at in 2:100) {
  dat <- aging_msm(dat, at)
  dat <- departure_msm(dat, at)
  dat <- arrival_msm(dat, at)
  dat <- partident_msm(dat, at)
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
  verbose.net(dat, "progress", at = at)
}

at = at + 1
dat.temp <- dat

dat.temp <- aging_msm(dat.temp, at)
dat.temp <- departure_msm(dat.temp, at)
dat.temp <- arrival_msm(dat.temp, at)

debug(partident_msm)
dat.temp <- partident_msm(dat.temp, at)
