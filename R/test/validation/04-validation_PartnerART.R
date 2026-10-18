## Model Schematic 4: Identified HIV+ Partners,
## Treatment Initiation and Reinitiation
## --------------------------------------------------------------- ##

suppressMessages(library(EpiModelHIV))
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")

sim <- list()

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
                   part.ident.start = 1,
                   prep.start = 1,
                   part.tx.init.prob = c(0, 0, 0),
                   part.tx.reinit.part.prob = c(0, 0, 0),
                   riskh.start = 1
)

init <- init_msm()

control <- control_msm(simno = 1001,
                       start = 1,
                       nsteps = 100,
                       ncores = 1,
                       nsims = 1)

set.seed(123)

## Burnin: Default model parameters; no treatment initiation for HIV+ partners
burnin <- netsim(est, param, init, control)

control1 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps = 200,
                        ncores = 1,
                        nsims = 5,
                        initialize.FUN = reinit_msm)

## ART Initiation

## Model 1: Default model parameters;
##          no treatment initiation for HIV+ partners
sim[[1]] <- netsim(burnin, param, init, control1)

## Model 2: Default model parameters;
##          complete treatment initiation for HIV+ partners
param$part.tx.init.prob <- c(1, 1, 1)
sim[[2]] <- netsim(burnin, param, init, control1)

## Model 3: Default model parameters;
##          average prob. of treatment initiation for HIV+ partners
param$part.tx.init.prob <- c(0.5, 0.5, 0.5)
sim[[3]] <- netsim(burnin, param, init, control1)

## Model 4: Default model parameters;
##          low prob. of treatment initiation for HIV+ partners
param$part.tx.init.prob <- c(0.01, 0.01, 0.01)
sim[[4]] <- netsim(burnin, param, init, control1)

## Model 5: Default model parameters;
##          low prob. of treatment initiation for HIV+ partners
param$part.tx.init.prob <- c(0.75, 0.75, 0.75)
sim[[5]] <- netsim(burnin, param, init, control1)

## Comparison
plot(sim[[1]], y = "part.init", ylim = c(0, 20),
     ylab = "Partners Initiating ART", main = "Validation: part.tx.init.prob")
plot(sim[[2]], y = "part.init",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "part.init",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "part.init",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim[[5]], y = "part.init",
     mean.col = "purple", qnts.col = "purple", add = TRUE)
legend(0, 20, legend = c("part.tx.init.prob = 1, 1, 1",
                         "part.tx.init.prob = 0.75, 0.75, 0.75",
                         "part.tx.init.prob = 0.5, 0.5, 0.5",
                         "part.tx.init.prob = 0.01, 0.01, 0.01",
                         "part.tx.init.prob = 0, 0, 0"),
       text.col = c("red", "purple", "green", "yellow", "blue"),
       cex = 0.75, bg = "lightblue")

## ART Renitiation

## Model 6: Default model parameters;
##          complete treatment reinitiation for HIV+ partners
param$part.tx.halt.prob <- c(1, 1, 1)
param$tx.halt.durable.rr <- c(1, 1, 1)
param$tx.halt.full.rr <- c(1, 1, 1)
param$part.tx.init.prob <- c(1, 1, 1)
param$part.tx.reinit.prob <- c(1, 1, 1)
sim[[6]] <- netsim(burnin, param, init, control1)

## Model 7: Default model parameters;
##          average prob. of treatment reinitiation for HIV+ partners
param$part.tx.reinit.prob <- c(0.5, 0.5, 0.5)
sim[[7]] <- netsim(burnin, param, init, control1)

## Model 8: Default model parameters;
##          low prob. of treatment reinitiation for HIV+ partners
param$part.tx.reinit.prob <- c(0.01, 0.01, 0.01)
sim[[8]] <- netsim(burnin, param, init, control1)

## Model 9: Default model parameters;
##          low prob. of treatment reinitiation for HIV+ partners
param$part.tx.reinit.prob <- c(0.75, 0.75, 0.75)
sim[[9]] <- netsim(burnin, param, init, control1)

## Comparison
plot(sim[[1]], y = "part.reinit", ylim = c(0, 10))
plot(sim[[6]], y = "part.reinit",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[7]], y = "part.reinit",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[8]], y = "part.reinit",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim[[9]], y = "part.reinit",
     mean.col = "purple", qnts.col = "purple", add = TRUE)

saveRDS(sim, file = "process4.sim.rds")
