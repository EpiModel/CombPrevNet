
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars()

## Parameters
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
burnin <- readRDS("est/burnin1.ATL.3race.rda")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   hiv.test.late.prob = c(0, 0, 0),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tt.part.supp = c(0, 0, 0),
                   tt.full.supp = c(1, 1, 1),
                   tt.dur.supp = c(0, 0, 0),
                   tx.halt.part.prob = c(0.0062, 0.0055, 0.0031),
                   tx.halt.full.rr = c(0.45, 0.45, 0.45),
                   tx.halt.dur.rr = c(0.45, 0.45, 0.45),
                   tx.reinit.part.prob = c(0.00255, 0.00255, 0.00255),
                   tx.reinit.full.rr = c(1, 1, 1),
                   tx.reinit.dur.rr = c(1, 1, 1),
                   max.time.off.tx.full.int = 52 * 15,
                   max.time.on.tx.part.int = 52 * 10,
                   max.time.off.tx.part.int = 52 * 10,
                   aids.mr = 1/250,
                   trans.scale = c(2.21, 0.405, 0.255),
                   acts.scale = 1.00,
                   acts.aids.vl = 5.75,
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.start.prob = 0.00411,
                   prep.require.lnt = FALSE,
                   prep.risk.reassess.method = "year")
init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)
control <- control_msm(simno = fsimno,
                       start = (52*60) + 1,
                       nsteps = 52*65,
                       nsims = ncores,
                       ncores = ncores,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE)

## Simulation
sim <- netsim(burnin, param, init, control)

# Merging
# savesim(sim, save.min = TRUE, save.max = FALSE)
savesim(sim, save.min = FALSE, save.max = TRUE, compress = FALSE, time.stamp = FALSE)

# process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
#                  truncate.at = 52*60)
