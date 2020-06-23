
## Packages
devtools::load_all("~/Dropbox/Dev/EpiModelHIV/EpiModelHIV-p")

## Parameters
netstats <- readRDS("est/netstats.rda")
epistats <- readRDS("est/epistats.rda")
# burnin <- readRDS("est/burnin.ATL.3race.FSonly.Prep15.rda")
burnin <- readRDS("est/burnin.ATL.3race.FSonly.rda")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   trans.scale = c(2.21, 0.405, 0.255),
                   prep.start = (52*60) + 1,
                   riskh.start = 52*59,
                   prep.start.prob = 0.712,
                   prep.require.lnt = TRUE,
                   prep.risk.reassess.method = "year",

                   prep.optim.start = (52*61) + 1,
                   prep.optim.init.prob = 0.005,
                   prep.start.prob.optim = 0.85,
                   prep.adhr.dist.optim = reallocate_pcp(c(0.089, 0.127, 0.784), 0.2),
                   prep.optim.adhr.cap = 30,
                   prep.optim.retn.cap = 30,
                   prep.discont.rate.optim = 1 - (2^(-1/(2*224.4237/7))))
init <- init_msm(prev.ugc = 0,
                 prev.rct = 0,
                 prev.rgc = 0,
                 prev.uct = 0)
control <- control_msm(simno = 1,
                       start = (52*60) + 1,
                       nsteps = 52*66,
                       nsims = 1,
                       ncores = 1,
                       initialize.FUN = reinit_msm,
                       save.nwstats = FALSE,
                       save.clin.hist = FALSE,
                       verbose = TRUE)

## Simulation
sim <- netsim(burnin, param, init, control)

View(param_msm)
View(prep_msm)
View(hivtrans_msm)
