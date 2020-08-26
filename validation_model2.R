## Model Schematic 2: Identified Partners Prep Initiation
## --------------------------------------------------------------- ##


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
                        prep.start.prob.part = 1,
                        prep.start = 1,
                        riskh.start = TRUE
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        start = 1,
                        nsteps =520,
                        ncores = 1,
                        nsims = 5,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE)

control2 <- control_msm(simno = 1001,
                        start = (52*70) + 1,
                        nsteps =52*80,
                        ncores = 1,
                        nsims = 5,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE, 
                        initialize.FUN = reinit_msm)

set.seed(123)

sim[[1]] <- netsim(est, param, init, control1)

## Full partner initiation of PreP once identified

param$prep.start.prob.part = 0
sim[[2]] <- netsim(est, param, init, control)



param$prep.start.prob.part = 0.5
sim[[2]] <- netsim(est, param, init, control)



param$prep.start.prob.part = 0.75
sim[[2]] <- netsim(est, param, init, control)


