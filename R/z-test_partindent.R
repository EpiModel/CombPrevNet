lnt <- TRUE
source("R/utils-params.R", local = TRUE)
orig <- readRDS("out/est/restart.rds")
orig$attr[[1]]$part.ident.counter <- rep(
  NA,
  length(orig$attr[[1]]$part.ident)
)
source("R/utils-scenarios.R", local = TRUE)

window_size <- 52

prep_start_time <- 52 * 61 + 1
test_params <- list(
  prep.start = prep_start_time,
  riskh.start = prep_start_time - 52,
  part.ident.start = 60 * 52 + 1,
  # Part ident parameters
  part.index.window = 0,
  part.index.degree = 1,
  part.index.prob = 0.75,
    part.ident.main.prob = 2 * param$part.ident.main.prob,
    part.ident.casl.prob = 2 * param$part.ident.casl.prob,
    part.ident.ooff.prob = 2 * param$part.ident.oof.prob,
  truncate.plist = 100,
  # see "R/z-indent_prob_calib.R"
  part.ident.main.prob = 1,
  part.ident.casl.prob = 1,
  part.ident.ooff.prob = 1,

  part.hiv.test.rate = rep(1, 3),
  part.prep.start.prob = rep(1, 3),
  part.tx.init.prob = rep(1, 3),
  part.tx.reinit.prob = rep(1, 3)
)

param <- update_params(param, test_params)

nsteps <- 100

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)


# debug(hivtest_msm)
sim <- netsim(orig, param, init, control)

# library(tidyverse)

# df <- as_tibble(sim) %>%
#   slice_tail(n = nsteps)

# df %>%
#   mutate(elic = found_indexes / elig_indexes) %>%
#   pull(elig_indexes) %>%
#   unique()
#   mean(na.rm = T)

# df %>%
#   select(
#     i = part_ident___ALL,
#     p = part_spos___ALL,
#     n = part_sneg___ALL,
#     r = part_prep___ALL,
#     t = part_txinit___ALL,
#     x = part_txreinit___ALL
#   ) %>%
#   print(n = nsteps)

# source("R/utils-ident_prop.R")
# ident_prob_calculator(target = 4, method = 2, pos = 1:3)
