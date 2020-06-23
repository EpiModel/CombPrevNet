
rm(list = ls())
# suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
est <- readRDS("est/netest.rda")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   hiv.test.late.prob = c(0, 0, 0),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tt.part.supp = c(0, 0, 0),
                   tt.full.supp = c(1, 1, 1),
                   tt.dur.supp = c(0, 0, 0),
                   tx.halt.part.prob = c(0.0062, 0.0055, 0.0031),
                   tx.halt.full.rr = c(0.45, 0.45, 0.45),
                   tx.halt.dur.rr = c(0.45, 0.45, 0.45),
                   tx.reinit.part.prob = c(0.00255, 0.00255, 0.00255),
                   tx.reinit.full.rr = c(1, 1, 1),
                   tx.reinit.dur.rr = c(1, 1, 1),
                   max.time.off.tx.full.int = 52 * 15,
                   max.time.on.tx.part.int = 52 * 10,
                   max.time.off.tx.part.int = 52 * 10,
                   aids.mr = 1/250,
                   trans.scale = c(2.21, 0.405, 0.255),
                   acts.scale = 1.00,
                   acts.aids.vl = 5.75,
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.start.prob = 0.66,
                   prep.require.lnt = TRUE,
                   prep.risk.reassess.method = "year")
init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)

devtools::load_all("~/Dropbox/Dev/EpiModelHIV/EpiModelHIV-p")
control <- control_msm(simno = 1,
                       nsteps = 52*5,
                       nsims = 1,
                       ncores = 1,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE)

sim <- netsim(est, param, init, control)

df <- as.data.frame(sim, out = "mean")
names(df)

summary(df$cc.dx.delay)
summary(df$cc.dx.delay.med)

df$cc.test.int

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y = "i.prev", mean.smooth = FALSE, ylim = c(0, 1))
plot(sim, y = "ir100", ylim = c(0, 10))
plot(sim, y = "num")
plot(sim, y = "age.mean")
plot(sim, y = "dep.gen", mean.smooth = TRUE)
plot(sim, y = "dep.AIDS", mean.smooth = FALSE)
plot(sim, y = "prepCurr")
plot(sim, y = "cc.dx", mean.smooth = FALSE)
plot(sim, y = "cc.linked", mean.smooth = FALSE)
plot(sim, y = "cc.linked1m", mean.smooth = FALSE)
plot(sim, y = "cc.tx", mean.smooth = FALSE)
plot(sim, y = "cc.tx.any1y", mean.smooth = FALSE)
plot(sim, y = "cc.vsupp", mean.smooth = FALSE)
plot(sim, y = "cc.vsupp.tt1", mean.smooth = FALSE)
plot(sim, y = "cc.vsupp.tt2", mean.smooth = FALSE)
plot(sim, y = "cc.vsupp.tt3", mean.smooth = FALSE)
plot(sim, y = "cc.vsupp.dur1y", mean.smooth = FALSE)

plot(sim, y = "cc.test.int", mean.smooth = FALSE)

plot(sim, y = "hstage.acute", mean.smooth = TRUE)
plot(sim, y = "hstage.chronic", mean.smooth = FALSE)
plot(sim, y = "hstage.aids", mean.smooth = FALSE)

plot(sim, y = "ir100.gc", mean.smooth = FALSE)
plot(sim, y = "ir100.ct", mean.smooth = FALSE)
plot(sim, y = "ir100.sti", mean.smooth = FALSE)
plot(sim, y = "prev.gc", mean.smooth = FALSE)
plot(sim, y = "prev.ct", mean.smooth = FALSE)

plot(sim, y = "R0.mean.cs", mean.smooth = FALSE)
plot(sim, y = "R0.mean.cens", mean.smooth = FALSE)

plot(sim, type = "formation", network = 1, plots.joined = FALSE, qnts = 1, mean.smooth = TRUE)
plot(sim, type = "formation", network = 2, plots.joined = FALSE, qnts = 1, mean.smooth = TRUE)
plot(sim, type = "formation", network = 3, plots.joined = FALSE, qnts = 1, mean.smooth = TRUE)

# include other nodefactor terms, check distribution of deg.tot, risk.grp over time

mean(tail(df$R0.mean.cens, 52), na.rm = TRUE)
mean(tail(df$R0.mean.cs, 52))


# Testing/Timing ------------------------------------------------------

m <- microbenchmark::microbenchmark(simnet_msm(dat, at = 2))
print(m, unit = "ms")

profvis::profvis(f(dat, at = 2), interval = 0.005)

dat <- initialize_msm(est, param, init, control, s = 1)

for (at in 2:52) {
  dat <- aging_msm(dat, at) # 1 ms
  dat <- departure_msm(dat, at) # 10 ms
  dat <- arrival_msm(dat, at) # 7 ms
  dat <- hivtest_msm(dat, at) # 2 ms
  dat <- hivtx_msm(dat, at) # 3 ms
  dat <- hivprogress_msm(dat, at) # 3 ms
  dat <- hivvl_msm(dat, at) # 5 ms
  dat <- simnet_msm(dat, at) # 86 ms
  dat <- acts_msm(dat, at) # 13 ms
  dat <- condoms_msm(dat, at) # 10 ms
  dat <- position_msm(dat, at) # 1 ms
  dat <- prep_msm(dat, at) # 8 ms
  dat <- hivtrans_msm(dat, at) # 3 ms
  dat <- stitrans_msm(dat, at) # 7 ms
  dat <- stirecov_msm(dat, at) # 4 ms
  dat <- stitx_msm(dat, at) # 6 ms
  dat <- prevalence_msm(dat, at) # 5 ms
  verbose.net(dat, "progress", at = at)
}

f <- function(dat, at) {
  dat <- aging_msm(dat, at) # 1 ms
  dat <- departure_msm(dat, at) # 10 ms
  dat <- arrival_msm(dat, at) # 7 ms
  dat <- hivtest_msm(dat, at) # 2 ms
  dat <- hivtx_msm(dat, at) # 3 ms
  dat <- hivprogress_msm(dat, at) # 3 ms
  dat <- hivvl_msm(dat, at) # 5 ms
  dat <- simnet_msm(dat, at) # 86 ms
  dat <- acts_msm(dat, at) # 13 ms
  dat <- condoms_msm(dat, at) # 10 ms
  dat <- position_msm(dat, at) # 1 ms
  dat <- prep_msm(dat, at) # 8 ms
  dat <- hivtrans_msm(dat, at) # 3 ms
  dat <- stitrans_msm(dat, at) # 7 ms
  dat <- stirecov_msm(dat, at) # 4 ms
  dat <- stitx_msm(dat, at) # 6 ms
  dat <- prevalence_msm(dat, at) # 5 ms
}

nrow(dat$temp$plist)
table(dat$temp$plist[, "start"])
table(dat$temp$plist[, "stop"])
head(dat$temp$plist)

plist <- as.data.frame(dat$temp$plist)
pmain <- filter(plist, ptype == 2)
table(pmain$start)
hist(pmain$start)
