library(EpiModelHIV)
# pkgload::load_all("../EpiModelHIV-p/")

# # Epi Trackers
# source("R/utils-epi_trackers.R")
# ls_trackers <- list(
#   s           = epi_s,
#   s_prep_elig = epi_s_prep_elig,
#   s_prep      = epi_s_prep,
#   i           = epi_i,
#   i_dx        = epi_i_dx,
#   i_tx        = epi_i_tx,
#   i_sup       = epi_i_sup,
#   i_sup_dur   = epi_i_sup_dur,
#   traj_1      = epi_tt_traj(1),
#   traj_2      = epi_tt_traj(2),
#   traj_3      = epi_tt_traj(3)
# )
# epi_trackers <- epi_tracker_by_race(ls_trackers, full = TRUE)
# param$epi_trackers <- c(
#   param$epi_trackers,
#   list(
#     "part_1" = epi_part_count(1),
#     "part_2" = epi_part_count(2),
#     "part_3" = epi_part_count(3))
# )
epi_trackers <- list()

# Params and inits
orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- rep(1, 3)

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  # Update on HIV natural history
  vl.acute.rise.int = 3,
  vl.acute.fall = 3,

  hiv.test.rate = c(0.00385, 0.00385, 0.0069),
  hiv.test.late.prob = rep(0, 3),
  tx.init.prob = c(0.1775, 0.19, 0.2521),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = rep(0, 3),
  tx.halt.partial.prob = c(0.0065, 0.0053, 0.003),
  tx.halt.full.rr = rep(0.45, 3),
  tx.halt.durable.rr = rep(0.45, 3),
  tx.reinit.partial.prob = rep(0.00255, 3),
  tx.reinit.full.rr = rep(1, 3),
  tx.reinit.dur.rr = rep(1, 3),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale =  c(2.95, 0.43333333, 0.3), #c(2.75, 0.4, 0.), #c(2.21, 0.405, 0.255),
  acts.scale = 1.00,
  acts.scale.main = 1.00,
  acts.scale.casl = 1.00,
  acts.aids.vl = 5.75,
  circ.prob = c(0.874, 0.874, 0.918),
  a.rate = 0.00052,
  prep.start = (52 * 60) + 1,
  riskh.start = 52 * 59,
  prep.adhr.dist = c(0.089, 0.127, 0.784),
  prep.adhr.hr = c(0.69, 0.19, 0.01),
  prep.start.prob =  rep(0.71, 3), # 0.00896,
  prep.discont.rate = rep(0.02138792, 3), # 1 - (2^(-1/(224.4237/7)))
  ## prep.tst.int = 90/7,         # do I need that?
  ## prep.risk.int = 182/7,       # do I need that?
  ## prep.sti.screen.int = 182/7,
  ## prep.sti.prob.tx = 1,
  prep.risk.reassess.method = "year",
  prep.require.lnt = TRUE, # FALSE -> start with random PrEP initiation

  ## STI PARAMS (default: from combprev2, make it gaps)
  ## Using values in prep-race: scripts/burnin/sim.burn.R
  ## If not mentionned -> default from prep disparities
  ## for H : mean(c(B, W))
  #ok
  rgc.tprob      = 0.2305246, # plogis(qlogis(0.19) + log(1.25)) #0.357,  # gaps appendix 9.4
  ugc.tprob      = 0.1933333, # 0.248,  # gaps appendix 9.4
  rct.tprob      = 0.208432, # plogis(qlogis(0.174) + log(1.25)) #0.3216, # gaps appendix 9.3
  uct.tprob      = 0.174, #0.213,  # gaps appendix 9.3
  rgc.sympt.prob = 0.1, #0.077, # gaps appendix 10.3
  ugc.sympt.prob = 0.9333333, #0.824, # gaps appendix 10.3
  rct.sympt.prob = 0.1, #0.1035,# gaps appendix 10.2
  uct.sympt.prob = 0.95, #0.885, # gaps appendix 10.2
  rgc.ntx.int    = 26, #35.11851, # gaps appendix 11.2
  ugc.ntx.int    = 26, #35.11851, # gaps appendix 11.2
  gc.tx.int      = 2, # gaps appendix 11.2 - mentionned, not explicit
  rct.ntx.int    = 32, #44.24538, # gaps appendix 11.1
  uct.ntx.int    = 32, #44.24538, # gaps appendix 11.1
  ct.tx.int      = 2, # gaps appendix 11.1 - mentionned, not explicit

  gc.sympt.prob.tx =  rep(0.9, 3),  #c(0.86, 0.91, 0.96),
  ct.sympt.prob.tx =  rep(0.9, 3),  #c(0.72, 0.785, 0.85),
  gc.asympt.prob.tx = rep(0.1, 3), #c(0.10, 0.145, 0.19),
  ct.asympt.prob.tx = rep(0.1, 3), #c(0.05, 0.525, 0.10),
  # gaps appendix 9.3 - 9.4 (not explained this way but similar result)
  sti.cond.eff = 0.95,
  sti.cond.fail = c(0.39, 0.3, 0.21),
  # gaps appendix 9.2
  hiv.rgc.rr = 2.78,
  hiv.ugc.rr = 1.73,
  hiv.rct.rr = 2.78,
  hiv.uct.rr = 1.73,
 # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
  hiv.dual.rr = 0.2, # not mentionned in appendix
  netresim.form.rr = rep(1, 3),
  netresim.disl.rr = rep(1, 2),

  # Part ident parameters
  part.ident.start = 65 * 52 + 1, # start ident after prep burnin
  part.index.window = 0,
  part.index.degree = 1,
  part.index.prob = 1,
  part.ident.main.window = 12,
  part.ident.casl.window = 12,
  part.ident.ooff.window = 12,
  # 0.432: number of partners reported in ATL
  # 2.1: number of partnes per node in the model see `epi_part_count`
  # c(1.04, 1.48, 3.91) number of partners
  part.ident.main.prob = 0.432 / 2.1,
  part.ident.casl.prob = 0.432 / 2.1,
  part.ident.ooff.prob = 0.432 / 2.1 * 0.5,
  part.hiv.test.rate = c(1, 1, 1),
  part.prep.start.prob = c(0.5, 0.5, 0.5),

  epi_trackers = epi_trackers
)

# Ensure that we do not truncate more than what is needed
param <- update_params(
  param, list(
    truncate.plist = max(
      param$part.ident.main.window,
      param$part.ident.casl.window,
      param$part.ident.ooff.window
    ) + 1
  )
)

## must be set by the calling script
if (lnt == FALSE) {
  param <- update_params(
    param, list(
      prep.require.lnt = FALSE,
      prep.start.prob = 0.00411
    )
  )
}

init <- init_msm(
  prev.ugc = 0.05,
  prev.rct = 0.05,
  prev.rgc = 0.05,
  prev.uct = 0.05
)

