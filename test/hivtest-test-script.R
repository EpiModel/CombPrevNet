# hivtest_msm debug script
# 

undebug(hivtest_msm)

set.seed(123)

dat <- initialize_msm(est, param, init, control, s = 1)

for (at in 2:9) {
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

at = at + 1
dat.temp <- dat

dat.temp <- aging_msm(dat.temp, at)
dat.temp <- departure_msm(dat.temp, at)
dat.temp <- arrival_msm(dat.temp, at)

debug(hivtest_msm)
dat.temp <- hivtest_msm(dat.temp, at)
