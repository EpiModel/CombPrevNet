library(EpiModelHIV)
## devtools::load_all("../EpiModelHIV-p/")

orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- rep(1, 3)

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  hiv.test.rate = c(0.00385, 0.00385, 0.0069),
  hiv.test.late.prob = rep(0, 3),
  tx.init.prob = c(0.1775, 0.19, 0.2521),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = rep(0, 3),
  tx.halt.part.prob = c(0.0065, 0.0053, 0.003),
  tx.halt.full.rr = rep(0.45, 3),
  tx.halt.dur.rr = rep(0.45, 3),
  tx.reinit.part.prob = rep(0.00255, 3),
  tx.reinit.full.rr = rep(1, 3),
  tx.reinit.dur.rr = rep(1, 3),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale =  c(2.7, 0.35, 0.243), #c(2.21, 0.405, 0.255),
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
  prep.start.prob =  0.71, # 0.00896,
  prep.discont.rate = 0.02138792, # 1 - (2^(-1/(224.4237/7)))
  ## prep.tst.int = 90/7,         # do I need that?
  ## prep.risk.int = 182/7,       # do I need that?
  ## prep.sti.screen.int = 182/7,
  ## prep.sti.prob.tx = 1,
  prep.risk.reassess.method = "year",
  prep.require.lnt = TRUE, # FALSE -> start with random PrEP initiation

  netresim.form.rr = rep(1, 3),
  netresim.disl.rr = rep(1, 2),

  # Part ident parameters
  part.ident.start = Inf,
  part.index.window = 0,
  part.ident.main.window = 12,
  part.ident.casl.window = 12,
  part.ident.ooff.window = 12,
  part.ident.main.prob = 1,
  part.ident.casl.prob = 1,
  part.ident.ooff.prob = 1,
  part.hiv.test.rate = c(1, 1, 1)
)

# Ensure that we do not truncate more than what is needed
param <- update_params(
  param, list(
    truncate.plist = max(
      param$part.ident.main.window,
      param$part.ident.casl.window,
      param$part.ident.ooff.window
    )
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
  prev.ugc = 0,
  prev.rct = 0,
  prev.rgc = 0,
  prev.uct = 0
)
