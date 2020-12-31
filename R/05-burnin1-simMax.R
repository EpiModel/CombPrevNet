
##
## 05. Epidemic Model Burnin, Stage 1, Full-Scale Simulation
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars()
ncore <- 4

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")
param <- readRDS("data/input/param.burnin1.rds")

init <- init_msm()
control <- control_msm(
  simno = fsimno,
  nsteps = 52 * 60,
  nsims = ncores,
  ncores = ncores,
  verbose = FALSE
)

## Simulation
sim <- netsim(est, param, init, control)

## Save-Max
savesim(sim, save.min = FALSE, save.max = TRUE,
        compress = TRUE, time.stamp = FALSE)

# saveRDS(sim, "data/output/sim.rda")
