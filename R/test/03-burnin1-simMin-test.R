
##
## 03. Epidemic Model Burnin, Stage 1, Parameter Calibration
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

## Packages
library("methods")
suppressMessages(
  library("EpiModelHIV")
)

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
                   riskh.start = 1,
                   prep.start = 26,
                   prep.start.prob = 0.66,

                   truncate.plist = 52,
                   part.ident.start = 52,
                   part.index.window = 0,
                   part.ident.main.window = 12,
                   part.ident.casl.window = 12,
                   part.ident.ooff.window = 12,
                   part.ident.main.prob = 0.5,
                   part.ident.casl.prob = 0.5,
                   part.ident.ooff.prob = 0.5,
                   part.hiv.test.rate = c(0.5, 0.5, 0.5),
                   part.prep.start.prob = 0.5,
                   part.tx.init.prob = c(0.6, 0.6, 0.8),
                   part.tx.halt.prob = c(0.00001, 0.00001, 0.00001),
                   part.tx.reinit.prob = c(0.05, 0.05, 0.05)
)
init <- init_msm()

pkgload::load_all("~/git/EpiModelHIV-p")
control <- control_msm(simno = fsimno,
                       nsteps = 52 * 2,
                       nsims = ncores,
                       ncores = ncores)

## Simulation
sim <- netsim(est, param, init, control)

## Save-Min
# savesim(sim, save.min = TRUE, save.max = FALSE, compress = TRUE)

# Merging
# process_simfiles(simno = simno, min.n = njobs, nsims = nsims)
