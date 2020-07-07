
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("C:/Users/smande6/Box Sync/est/netstats.rda")
epistats <- readRDS("C:/Users/smande6/Box Sync/est/epistats.rda")
est <- readRDS("C:/Users/smande6/Box Sync/est/netest.rda")

param <- param_msm(netstats = netstats,
                  epistats = epistats,
                  hiv.test.rate = c(0.00432, 0.00425, 0.00730),
                  hiv.test.late.prob = c(0, 0, 0),
                  tx.init.prob = c(0.1775, 0.190, 0.2521),
                  tt.part.supp = c(0.45, 0.40, 0.28),
                  tt.full.supp = c(0.55, 0.60, 0.72),
                  tt.dur.supp = c(0, 0, 0),
                  tx.halt.part.prob = c(0.009, 0.0084, 0.00768),
                  tx.halt.full.rr = c(0.45, 0.45, 0.45),
                  tx.halt.dur.rr = c(0.45, 0.45, 0.45),
                  tx.reinit.part.prob = c(0.0115, 0.0135, 0.0205),
                  tx.reinit.full.rr = c(1, 1, 1),
                  tx.reinit.dur.rr = c(1, 1, 1),
                  max.time.off.tx.full.int = 52 * 15,
                  max.time.on.tx.part.int = 52 * 10,
                  max.time.off.tx.part.int = 52 * 10,
                  aids.mr = 1/250,
                  trans.scale = c(2.77, 0.47, 0.29),
                  acts.scale = 1.00,
                  acts.aids.vl = 5.75,
                  prep.start = (52*60) + 1,
                  riskh.start = 52*59,
                  prep.start.prob = 0.66,
                  prep.require.lnt = TRUE,
                  prep.risk.reassess.method = "year")
init <- init_msm()
control <- control_msm(simno = 1,
                       nsteps = 52 * 2,
                       nsims = 1,
                       ncores = 1,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       tergmLite = TRUE)

sim <- netsim(est, param, init, control)

set.seed(312)

dat <- initialize_msm(est, param, init, control, s = 1)

for (at in 2:104) {
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
  verbose.net(dat, "progress", at = at)
}

nrow(dat$temp$plist)
table(dat$temp$plist[, "start"])
table(dat$temp$plist[, "stop"])
head(dat$temp$plist)

plist <- as.data.frame(dat$temp$plist)
pmain <- filter(plist, ptype == 2)
table(pmain$start)
hist(pmain$start)
