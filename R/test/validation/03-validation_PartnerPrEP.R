## Model Process 3: Identified Partners Prep Initiation
## --------------------------------------------------------------- ##

suppressMessages(library(EpiModelHIV))
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")

param <- param_msm(epistats = epistats,
                        netstats = netstats,
                        part.hiv.test.rate = c(1, 1, 1),
                        part.identification = 1,
                        part.lookback.main = 52,
                        part.lookback.casl = 26,
                        part.lookback.ooff = 4,
                        part.ident.main = 1,
                        part.ident.casl = 1,
                        part.ident.ooff = 1,
                        part.prep.start.prob = 1,
                        prep.start = 101,
                        part.prep.start = 101,
                        riskh.start = 1
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        start = 1,
                        nsteps = 100,
                        ncores = 1,
                        nsims = 1)

control2 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps = 200,
                        ncores = 1,
                        nsims = 10,
                        initialize.FUN = reinit_msm)

set.seed(123)

burnin <- netsim(est, param, init, control1)

sim <- list()

sim[[1]] <- netsim(burnin, param, init, control2)

## Average prob. of initiating PreP
param$part.prep.start.prob <- 0.5
sim[[2]] <- netsim(burnin, param, init, control2)

## High prob. of initiating PreP
param$part.prep.start.prob <- 0.75
sim[[3]] <- netsim(burnin, param, init, control2)

## Full partner initiation of PreP once identified
param$part.prep.start.prob <- 1
sim[[4]] <- netsim(burnin, param, init, control2)

## Delay prep intervention for parnters
param$part.prep.start <- 150
sim[[5]] <- netsim(burnin, param, init, control2)

plot(sim[[1]], y = "prep.part", xlim = c(0, 200), ylim = c(0, 150),
     ylab = "Total PreP Initiation", main = "Validation: PreP Initiation")
plot(sim[[2]], y = "prep.part",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "prep.part",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "prep.part",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim[[5]], y = "prep.part",
     mean.col = "purple", qnts.col = "purple", add = TRUE)
legend(0, 140, legend = c("part.prep.start.prob = 0, 0, 0",
                          "part.prep.start.prob = 0.5, 0.5, 0.5",
                          "part.prep.start.prob = 0.75, 0.75, 0.75",
                          "part.prep.start.prob = 1, 1, 1",
                          "part.prep.start = 150"),
       text.col = c("blue", "red", "green", "yellow", "purple"),
       cex = 0.75, bg = "lightblue")

saveRDS(sim, file = "process3.sim.rds")
