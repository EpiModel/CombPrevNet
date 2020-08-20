## Model Schematic 1: Identification of Partners of Incident HIV+ MSM
## --------------------------------------------------------------- ##


netstats <- readRDS("C:/Users/smande6/Box Sync/est/netstats.rda")
epistats <- readRDS("C:/Users/smande6/Box Sync/est/epistats.rda")
est <- readRDS("C:/Users/smande6/Box Sync/est/netest.rda")


## part.lookback: Rates of elicitation for partner lookback period:
# No elicitation of partners (i.e., set lookback window to 0 time steps).
#
param.orig <- param_msm(epistats = epistats,
                   netstats = netstats,
                   hiv.scrn.rate = c(1, 1, 1),
                   part.lookback.main = 0,
                   part.lookback.casl = 0,
                   part.lookback.ooff = 0,
                   part.ident.main = 1,
                   part.ident.casl = 1,
                   part.ident.ooff = 1,
                   ptype.lookup = c(1, 2, 3),
                   prep.start.prob.part = 1,
)

init <- init_msm()

control <- control_msm(simno = 1,
                       nsteps = 52*10 ,
                       nsims = 5,
                       ncores = 1,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       tergmLite = TRUE)

sim <- list()

set.seed(123)

sim[[1]] <- netsim(est, param.orig, init, control)

# One year partner lookback for all partnership types.
#
param <- param.orig
param$part.lookback.main <- 52
param$part.lookback.casl <- 52
param$part.lookback.ooff <- 52

sim[[2]] <- netsim(est, param, init, control)

# Realistic breakdown of partnership types
# 
param$part.lookback.main <- 52
param$part.lookback.casl <- 26
param$part.lookback.ooff <- 4

sim[[3]] <- netsim(est, param, init, control)

## part.ident: probability of identifying eligible partner
# No prob. of success in identification. Note: should be equivalent to sim1.
param$part.ident.main <- 0
param$part.ident.casl <- 0
param$part.ident.ooff <- 0

sim[[4]] <- netsim(est, param, init, control)

# Complete partner identification
param$part.ident.main <- 1
param$part.ident.casl <- 1
param$part.ident.ooff <- 1

sim[[5]] <- netsim(est, param, init,control)

# Stratified partner identification rates - high
param$part.ident.main <- 0.75
param$part.ident.casl <- 0.6
param$part.ident.ooff <- 0.3

sim[[6]] <- netsim(est, param, init, control)

# Stratified partner identification rates - low
param$part.ident.main <- 0.03
param$part.ident.casl <- 0.1
param$part.ident.ooff <- 0.05

sim[[7]] <- netsim(est, param, init, control)

# Stratified partner identification rates - high main/casl recall,
# low one off partner recall
param$part.ident.main <- 0.9
param$part.ident.casl <- 0.8
param$part.ident.ooff <- 0.05

sim[[8]] <- netsim(est, param, init, control)

# Stratified partner identification rates - low main/casl recall,
# high one off partner recall
param$part.ident.main <- 0.3
param$part.ident.casl <- 0.1
param$part.ident.ooff <- 0.8

sim[[9]] <- netsim(est, param, init, control)

## ptype.lookup: subset by type of relationship
# Only main/casual
param$part.ident.main <- 0.75
param$part.ident.casl <- 0.6
param$part.ident.ooff <- 0.3
param$ptype.lookup <- c(1, 2)

sim[[10]] <- netsim(est, param, init, control)

# Only casual
param$ptype.lookup <- 3

sim[[11]] <- netsim(est, param, init, control)

## hiv.scrn.rate: HIV testing rate for partners of individuas who are incident HIV+.
# Low screen rate
param$hiv.scrn.rate <- c(0.5, 0.25, 0.125)

sim[[12]] <- netsim(est, param, init, control)

# High screen rate
param$hiv.scrn.rate <- c(0.95, 0.75, 0.5)

sim[[13]] <- netsim(est, param, init, control)

saveRDS(sim, file = "model1.sim.rda")
