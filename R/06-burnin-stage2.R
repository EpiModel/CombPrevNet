
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars()

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
burnin <- readRDS("data/input/burnin.ATL.3race.rds")
param <- readRDS("data/input/param.burnin1.rds")

init <- init_msm()
control <- control_msm(simno = fsimno,
                       start = (52*60) + 1,
                       nsteps = 52*65,
                       nsims = ncores,
                       ncores = ncores,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE)

# Update parameters


## Simulation
sim <- netsim(burnin, param, init, control)

# Merging
# savesim(sim, save.min = TRUE, save.max = FALSE)
savesim(sim, save.min = FALSE, save.max = TRUE, compress = FALSE, time.stamp = FALSE)

# process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
#                  truncate.at = 52*60)
