
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Environmental Arguments
pull_env_vars(num.vars = c("HTRB", "HTRH", "HTRW",
                           "TIPB", "TIPH", "TIPW",
                           "THPB", "THPH", "THPW"),
              logic.vars = "LNT")

## Parameters
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
if (LNT == TRUE) {
  burnin <- readRDS("est/burnin.ATL.3race.FSonly.Prep15.rda")
  PSP <- 0.712
} else {
  burnin <- readRDS("est/burnin.ATL.3race.FSonly.Prep15-noLNT.rda")
  PSP <- 0.00411
}

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(HTRB, HTRH, HTRW),
                   hiv.test.late.prob = c(0, 0, 0),
                   tx.init.prob = c(TIPB, TIPH, TIPW),
                   tt.part.supp = c(0, 0, 0),
                   tt.full.supp = c(1, 1, 1),
                   tt.dur.supp = c(0, 0, 0),
                   tx.halt.part.prob = c(THPB, THPH, THPW),
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
                   prep.start.prob = PSP,
                   prep.require.lnt = LNT,
                   prep.risk.reassess.method = "year")
init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)
control <- control_msm(simno = fsimno,
                       start = (52*65) + 1,
                       nsteps = 52*75,
                       nsims = ncores,
                       ncores = ncores,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       verbose = FALSE)

## Simulation
sim <- netsim(burnin, param, init, control)

# Merging
savesim(sim, save.min = TRUE, save.max = FALSE, compress = TRUE)
# process_simfiles(simno = simno, min.n = njobs, nsims = nsims,
#                  truncate.at = 52*65, compress = FALSE)
