library(EpiModelHIV)
library(tidyverse)

lnt <- TRUE
source("R/utils-params.R", local = TRUE)
orig <- readRDS("out/est/restart.rds")

test_params <- list(
  part.ident.start = 60 * 52 + 1,
  # Part ident parameters
  part.index.window = 0,
  part.index.degree = 1,
  part.index.prob = 1,
  part.ident.main.window = 12,
  part.ident.casl.window = 12,
  part.ident.ooff.window = 12,
  # see "R/z-indent_prob_calib.R"
  part.ident.main.prob = 1,
  part.ident.casl.prob = 1,
  part.ident.ooff.prob = 1,
  part.hiv.test.rate = rep(1, 3),
  part.prep.start.prob = rep(0.5, 3),
  part.tx.init.prob = c(0.6, 0.6, 0.8),
  part.tx.halt.prob = c(0.00102, 0.00102, 0.00071),
  part.tx.reinit.prob = rep(0.5, 3)
)

param <- update_params(param, test_params)

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 65 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = TRUE,
  raw_output = TRUE
)


debug(partident_msm)
sim <- netsim(orig, param, init, control)

df <- as_tibble(process_out.net(sim[[1]]))

# next step
#
# - prints in partindent
#   - how many new diag
#   - selected a different lvls (degree, window, prob)
#   - at least one print per section that can be skiped
