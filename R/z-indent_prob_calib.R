## The observed number of identified partners in Atlanta is 0.432 and
## the observed number of partners in my simulations is
## x <- c(1.1, 1.6, 9.7) (main, casl, oof)
##
## with a 52 week window and the `epi_partner_count` epi tracker
##
##
nsims <- ncores <- 1
lnt <- TRUE
source("R/utils-params.R")
orig <- readRDS("out/est/restart.rds")
orig$attr[[1]]$part.ident.counter <- rep(NA, length(orig$attr[[1]]$part.ident))
orig$attr[[1]]$prep.start.counter <- rep(NA, length(orig$attr[[1]]$part.ident))

param$epi_trackers <- c(param$epi_trackers, "partner_count" = epi_partner_count)

control <- control_msm(
  simno = 1,
  start = 60 * 52 + 1,
  nsteps = 80 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
  nsims = ncores,
  ncores = ncores,
  initialize.FUN = reinit_msm,
  save.nwstats = FALSE,
  # raw.output = TRUE,
  raw.output = FALSE,
  verbose = FALSE
)

## Simulation
# debug(partident_msm)
sim <- netsim(orig, param, init, control)

library(dplyr)

df <- as.data.frame(sim)

df %>%
  filter(time > 52 * 70) %>%
  select(partner_count, elig_indexes) %>%
  mutate(
    p1 = partner_count %/% 1e3^0 %% 1e3,
    p2 = partner_count %/% 1e3^1 %% 1e3,
    p3 = partner_count %/% 1e3^2 %% 1e3
  ) %>%
  summarize(across(c(p1, p2, p3, elig_indexes), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(c(p1, p2, p3), ~ .x / elig_indexes))

x <- c(0.7, 1, 6)
target_ident <- 0.432

## option 1 - we enforce part.indent.main.prob == part.indent.casl.prob == 2 * part.indent.oof.prob
p_temp <- target_ident / sum(x * c(1, 1, 1/2))
p_temp
# [1] 0.09191489
p <- c(p_temp, p_temp, p_temp / 2)
p
# [1] 0.09191489 0.09191489 0.04595745
p * x
# [1] 0.06434043 0.09191489 0.27574468
sum(p * x)
# [1] 0.432

## option 2
##
## we force each partnership type to give the same "number" of partners
p <- target_ident / 3 / x
p
# [1] 0.2057143 0.1440000 0.0240000
p * x
# [1] 0.144 0.144 0.144# validation
sum(p * x)
# [1] 0.432
