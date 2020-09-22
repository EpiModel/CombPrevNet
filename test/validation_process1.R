## Process Schematic 1: Identification of Partners of Incident HIV+ MSM
## --------------------------------------------------------------- ##

suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("est/netstats.rds")
epistats <- readRDS("est/epistats.rds")
est <- readRDS("est/netest.rds")


## part.lookback: Rates of elicitation for partner lookback period:
# One year elicitation of partners (i.e., set lookback window to 52 time steps).
#
param <- param_msm(epistats = epistats,
                   netstats = netstats,
                   hiv.scrn.rate = c(1, 1, 1),
                   part.identification = 1,
                   part.lookback.main = 52,
                   part.lookback.casl = 52,
                   part.lookback.ooff = 52,
                   part.ident.main = 1,
                   part.ident.casl = 1,
                   part.ident.ooff = 1,
                   ptype.lookup = c(1, 2, 3)
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        nsteps = 100,
                        ncores = 1,
                        nsims = 1,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE)

sim <- list()

set.seed(123)

burnin <- netsim(est, param, init, control1)



control2 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps = 200,
                        ncores = 1,
                        nsims = 5,
                        save.nwstats = FALSE,
                        save.clin.hist = FALSE,
                        tergmLite = TRUE, 
                        initialize.FUN = reinit_msm)


sim[[1]] <- netsim(burnin, param, init, control2)


# Realistic breakdown of partnership types
param$part.lookback.main <- 52
param$part.lookback.casl <- 26
param$part.lookback.ooff <- 4

sim[[2]] <- netsim(burnin, param, init, control2)

# Longer recall periods
# 
param$part.lookback.main <- 104
param$part.lookback.casl <- 52
param$part.lookback.ooff <- 26

sim[[3]] <- netsim(burnin, param, init, control2)

# Very short recall periods
# 
param$part.lookback.main <- 12
param$part.lookback.casl <- 6
param$part.lookback.ooff <- 2

sim[[4]] <- netsim(burnin, param, init, control2)

plot(sim[[1]], y = "part.identified", xlim = c(100, 200), xlab = "Timestep", ylab = "Partners Identified", main = "Validation: part.lookback")
plot(sim[[2]], y = "part.identified", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "part.identified", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "part.identified", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(100, 250, legend=c("part.lookback = 52, 52, 52", "part.lookback = 52, 26, 4",
                         "part.lookback = 104, 52, 26", "part.lookback = 12, 6, 2"),
       text.col=c("blue", "red", "green", "yellow"), cex=0.75, bg = "lightblue")

## part.ident: probability of identifying eligible partner
param$part.lookback.main <- 52
param$part.lookback.casl <- 26
param$part.lookback.ooff <- 4

param$part.ident.main <- 0
param$part.ident.casl <- 0
param$part.ident.ooff <- 0

sim[[5]] <- netsim(burnin, param, init, control2)

# Complete partner identification
param$part.ident.main <- 1
param$part.ident.casl <- 1
param$part.ident.ooff <- 1

sim[[6]] <- netsim(burnin, param, init,control2)

# Stratified partner identification rates - high
param$part.ident.main <- 0.75
param$part.ident.casl <- 0.6
param$part.ident.ooff <- 0.3

sim[[7]] <- netsim(burnin, param, init, control2)

# Stratified partner identification rates - low
param$part.ident.main <- 0.03
param$part.ident.casl <- 0.1
param$part.ident.ooff <- 0.05

sim[[8]] <- netsim(burnin, param, init, control2)

plot(sim[[5]], y = "part.identified", xlim = c(100, 200), ylim = c(0, 250), xlab = "Timestep", ylab = "Partners Identified", main = "Validation: part.ident")
plot(sim[[6]], y = "part.identified", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[7]], y = "part.identified", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[8]], y = "part.identified", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(100, 200, legend = c("part.ident = 0, 0, 0", "part.ident = 1, 1, 1",
                           "part.ident = 0.75, 0.6, 0.3", "part.ident = 0.03, 0.1, 0.05"),
       text.col = c("blue", "red", "green", "yellow"), cex=0.75, bg = "lightblue")

# Stratified partner identification rates - high main/casl recall,
# low one off partner recall
param$part.ident.main <- 0.9
param$part.ident.casl <- 0.8
param$part.ident.ooff <- 0.05

sim[[9]] <- netsim(burnin, param, init, control2)

# Stratified partner identification rates - low main/casl recall,
# high one off partner recall
param$part.ident.main <- 0.3
param$part.ident.casl <- 0.1
param$part.ident.ooff <- 0.8

sim[[10]] <- netsim(burnin, param, init, control2)

plot(sim[[5]], y = "part.identified", xlim = c(100, 200), ylim = c(0, 250), xlab = "Timestep", ylab = "Partners Identified", main = "Validation: part.ident")
plot(sim[[9]], y = "part.identified", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[10]], y = "part.identified", mean.col = "green", qnts.col = "green", add = TRUE)
legend(100, 250, legend = c("part.ident = 0, 0, 0", "part.ident = 0.9, 0.8, 0.05",
                           "part.ident = 0.3, 0.1, 0.9"),
       text.col = c("blue", "red", "green"), cex=0.75, bg = "lightblue")

## ptype.lookup: subset by type of relationship
# Only main/casual
param$part.ident.main <- 0.75
param$part.ident.casl <- 0.6
param$part.ident.ooff <- 0.3
param$ptype.lookup <- c(1, 2)

sim[[11]] <- netsim(burnin, param, init, control2)

# Only casual
param$ptype.lookup <- 3

sim[[12]] <- netsim(burnin, param, init, control2)

plot(sim[[7]], y = "part.identified", xlim = c(100, 200), ylim = c(0, 250), ylab = "Partners Identified", main = "Validation: ptype.lookup", xlab = "Timestep")
plot(sim[[11]], y = "part.identified", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[12]], y = "part.identified", mean.col = "green", qnts.col = "green", add = TRUE)
legend(100, 250, legend = c("ptype.lookup = 1, 2, 3", "ptype.lookup = 1, 2",
                           "ptype.lookup = 3"),
       text.col = c("blue", "red", "green"), cex=0.75, bg = "lightblue")

saveRDS(sim, file = "process1.sim.rds")
