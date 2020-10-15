
##
## 03. Epidemic Model Burnin, Stage 1, Parameter Calibration
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##


## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars()

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
                   tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
                   trans.scale = c(2.44, 0.424, 0.270),
                   riskh.start = 52 * 59,
                   prep.start = (52 * 60) + 1,
                   prep.start.prob = 0.66)
init <- init_msm()
control <- control_msm(simno = fsimno,
                       nsteps = 52 * 60,
                       nsims = ncores,
                       ncores = ncores)
## Simulation
sim <- netsim(est, param, init, control)

## Save-Min
savesim(sim, save.min = TRUE, save.max = FALSE, compress = TRUE)

# Merging
process_simfiles(simno = simno, min.n = njobs, nsims = nsims)
