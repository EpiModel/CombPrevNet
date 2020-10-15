## Process Schematic 1: Identification of Partners of Incident HIV+ MSM
## --------------------------------------------------------------- ##

suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")


## part.ident: Rates of elicitation for partner lookback period:
# One year elicitation of partners (i.e., set lookback window to 52 time steps).
#
param <- param_msm(epistats = epistats,
                   netstats = netstats,
                   part.hiv.test.rate  = c(1, 1, 1),
                   part.ident.main.window =  52,
                   part.ident.casl.window  = 52,
                   part.ident.ooff.window  = 52,
                   part.ident.main.prob = 1,
                   part.ident.casl.prob = 1,
                   part.ident.ooff.prob = 1,
                   part.index.window = Inf,
                   part.ident.start = Inf
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        nsteps = 100,
                        ncores = 1,
                        nsims = 1)

sim <- list()

set.seed(123)

burnin <- netsim(est, param, init, control1)



control2 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps = 200,
                        ncores = 1,
                        nsims = 5,
                        initialize.FUN = reinit_msm)


sim[[1]] <- netsim(burnin, param, init, control2)


# Realistic breakdown of partnership types
param$part.ident.main.window <- 52
param$part.ident.casl.window <- 26
param$part.ident.ooff.window <- 4

sim[[2]] <- netsim(burnin, param, init, control2)

# Longer recall periods
#
param$part.ident.main.window <- 104
param$part.ident.casl.window <- 52
param$part.ident.ooff.window <- 26

sim[[3]] <- netsim(burnin, param, init, control2)

# Very short recall periods
#
param$part.ident.main.window <- 12
param$part.ident.casl.window <- 6
param$part.ident.ooff.window <- 2

sim[[4]] <- netsim(burnin, param, init, control2)

plot(sim[[1]], y = "part.identified", xlim = c(100, 200), xlab = "Timestep",
     ylab = "Partners Identified", main = "Validation: part.ident")
plot(sim[[2]], y = "part.identified",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "part.identified",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "part.identified",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(100, 450, legend = c("part.ident = 52, 52, 52",
                            "part.ident = 52, 26, 4",
                            "part.ident = 104, 52, 26",
                            "part.ident = 12, 6, 2"),
       text.col = c("blue", "red", "green", "yellow"),
       cex = 0.75, bg = "lightblue")

## part.ident: probability of identifying eligible partner
param$part.ident.main.window <- 52
param$part.ident.casl <- 26
param$part.ident.ooff <- 4

param$part.ident.main.window <- 0
param$part.ident.casl <- 0
param$part.ident.ooff <- 0

sim[[5]] <- netsim(burnin, param, init, control2)

# Complete partner identification
param$part.ident.main.prob <- 1
param$part.ident.casl.prob <- 1
param$part.ident.ooff.prob <- 1

sim[[6]] <- netsim(burnin, param, init, control2)

# Stratified partner identification rates - high
param$part.ident.main.prob <- 0.75
param$part.ident.casl.prob <- 0.6
param$part.ident.ooff.prob <- 0.3

sim[[7]] <- netsim(burnin, param, init, control2)

# Stratified partner identification rates - low
param$part.ident.main.prob <- 0.03
param$part.ident.casl.prob <- 0.1
param$part.ident.ooff.prob <- 0.05

sim[[8]] <- netsim(burnin, param, init, control2)

plot(sim[[5]], y = "part.identified", xlim = c(100, 200), ylim = c(0, 250),
     xlab = "Timestep", ylab = "Partners Identified",
     main = "Validation: part.ident.prob")
plot(sim[[6]], y = "part.identified",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[7]], y = "part.identified",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[8]], y = "part.identified",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(100, 200, legend = c("part.ident.prob = 0, 0, 0",
                            "part.ident.prob = 1, 1, 1",
                           "part.ident.prob = 0.75, 0.6, 0.3",
                           "part.ident.prob = 0.03, 0.1, 0.05"),
       text.col = c("blue", "red", "green", "yellow"),
       cex = 0.75, bg = "lightblue")

# Stratified partner identification rates - high main/casl recall,
# low one off partner recall
param$part.ident.main.prob <- 0.9
param$part.ident.casl.prob <- 0.8
param$part.ident.ooff.prob <- 0.05

sim[[9]] <- netsim(burnin, param, init, control2)

# Stratified partner identification rates - low main/casl recall,
# high one off partner recall
param$part.ident.main.prob <- 0.3
param$part.ident.casl.prob <- 0.1
param$part.ident.ooff.prob <- 0.8

sim[[10]] <- netsim(burnin, param, init, control2)

plot(sim[[5]], y = "part.identified", xlim = c(100, 200), ylim = c(0, 250),
     xlab = "Timestep", ylab = "Partners Identified",
     main = "Validation: part.ident")
plot(sim[[9]], y = "part.identified",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[10]], y = "part.identified",
     mean.col = "green", qnts.col = "green", add = TRUE)
legend(100, 250, legend = c("part.ident.prob = 0, 0, 0",
                            "part.ident.prob = 0.9, 0.8, 0.05",
                           "part.ident.prob = 0.3, 0.1, 0.9"),
       text.col = c("blue", "red", "green"),
       cex = 0.75, bg = "lightblue")

saveRDS(sim, file = "process1.sim.rds")
