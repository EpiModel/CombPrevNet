
##
## 05. Epidemic Model Burnin, Stage 1, Full-Scale Simulation
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))
suppressMessages(library("EpiModelHPC"))

## Environmental Arguments
pull_env_vars()

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")
param <- readRDS("data/input/param.burnin1.rds")

init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)
control <- control_msm(simno = fsimno,
                       nsteps = 52*60,
                       nsims = ncores,
                       ncores = ncores,
                       tergmLite = TRUE)
## Simulation
sim <- netsim(est, param, init, control)

## Save-Max
savesim(sim, save.min = FALSE, save.max = TRUE, compress = TRUE, time.stamp = FALSE)
