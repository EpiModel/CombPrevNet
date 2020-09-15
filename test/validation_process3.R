## Model Process 3: Identified Partners Prep Initiation
## --------------------------------------------------------------- ##

suppressMessages(library(EpiModelHIV))
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
est <- readRDS("est/netest.rda")

param <- param_msm(epistats = epistats,
                        netstats = netstats,
                        hiv.scrn.rate = c(1, 1, 1),
                        part.lookback.main = 52,
                        part.lookback.casl = 26,
                        part.lookback.ooff = 4,
                        part.ident.main = 1,
                        part.ident.casl = 1,
                        part.ident.ooff = 1,
                        ptype.lookup = c(1, 2, 3),
                        prep.start.prob.part = 0,
                        prep.start = 1,
                        riskh.start = TRUE
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        start = 1,
                        nsteps =100,
                        ncores = 1,
                        nsims = 1,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE)

control2 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps =200,
                        ncores = 1,
                        nsims = 10,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE, 
                        initialize.FUN = reinit_msm)

set.seed(123)

burnin <- netsim(est, param, init, control1)

sim <- list()

sim[[1]] <- netsim(burnin, param, init, control2)

## Average prob. of initiating PreP
param$prep.start.prob.part = 0.5
sim[[2]] <- netsim(burnin, param, init, control2)

## High prob. of initiating PreP
param$prep.start.prob.part = 0.75
sim[[3]] <- netsim(burnin, param, init, control2)

## Full partner initiation of PreP once identified
param$prep.start.prob.part = 1
sim[[4]] <- netsim(burnin, param, init, control2)

plot(sim[[1]], y = "prep.part", xlim = c(0, 300), ylim = c(0, 50), ylab = "Total PreP Initiation", main = "Validation: prep.start.prob.part")
plot(sim[[2]], y = "prep.part", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "prep.part", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "prep.part", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(0, 40, legend=c("prep.start.prob.part = 0, 0, 0", "prep.start.prob.part = 0.5, 0.5, 0.5",
                          "prep.start.prob.part = 0.75, 0.75, 0.75", "prep.start.prob.part = 1, 1, 1"), 
       text.col=c("blue", "red", "green", "yellow"), cex=0.75, bg = "lightblue")

saveRDS(sim, file = "process3.sim.rda")

