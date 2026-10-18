
##
## 07. Epidemic Model Burnin, Stage 2, Parameter Calibration
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
burnin <- readRDS("data/input/burnin1.rds")
param <- readRDS("data/input/param.burnin1.rds")

init <- init_msm()
control <- control_msm(simno = fsimno,
                       start = (52 * 60) + 1,
                       nsteps = 52 * 65,
                       nsims = ncores,
                       ncores = ncores,
                       initialize.FUN = reinit_msm)

# Intervention parameters
interv_params <- list(
  part.ident.start = Inf,
  part.index.window = 0,
  part.index.degree = 1,
  part.ident.main.window = 12,
  part.ident.casl.window = 12,
  part.ident.ooff.window = 12,
  part.ident.main.prob = 1,
  part.ident.casl.prob = 1,
  part.ident.ooff.prob = 1,
  part.hiv.test.rate = c(1, 1, 1),
  part.prep.start.prob = 0.5,
  part.tx.init.prob = c(0.6, 0.6, 0.8),
  part.tx.halt.prob = c(0.00102, 0.00102, 0.00071),
  part.tx.reinit.prob = c(0.5, 0.5, 0.5)
)
param <- update_params(param, interv_params)


## Simulation
sim <- netsim(burnin, param, init, control)

# Merging
savesim(sim, save.min = TRUE, save.max = FALSE)

process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
                 truncate.at = 52 * 60)
