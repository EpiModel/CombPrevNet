## Model Schematic 2: Identified Partners Prep Initiation
## --------------------------------------------------------------- ##


netstats <- readRDS("C:/Users/smande6/Box Sync/est/netstats.rda")
epistats <- readRDS("C:/Users/smande6/Box Sync/est/epistats.rda")
est <- readRDS("C:/Users/smande6/Box Sync/est/netest.rda")

sim <- list()
param.orig <- param_msm(epistats = epistats,
                        netstats = netstats,
                        hiv.scrn.rate = c(1, 1, 1),
                        part.lookback.main = 52,
                        part.lookback.casl = 26,
                        part.lookback.ooff = 4,
                        part.ident.main = 1,
                        part.ident.casl = 1,
                        part.ident.ooff = 1,
                        ptype.lookup = c(1, 2, 3),
                        prep.start.prob.part = 1,
)

init <- init_msm()

control <- control_msm(simno = 1,
                       nsteps = 52*10 ,
                       nsims = 1,
                       ncores = 1,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       tergmLite = TRUE)

sim <- list()

set.seed(123)

sim[[1]] <- netsim(est, param.orig, init, control)

## Full partner initiation of PreP once identified

param$prep.start.prob.part = 1
sim[[2]] <- netsim(est, param, init, control)

plot(sim[[2]], y = c("prepELig", "prepCurr"))
plot(sim[[1]], y = c("prepElig", "prepCurr"), mean.col = c("green", "yellow"),
     qnts.col = c("green", "yellow"))


