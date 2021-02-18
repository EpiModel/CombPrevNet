## The observed number of identified partners in Atlanta is 0.432 and
## the observed number of partners in my simulations is
## x <- c(1.1, 1.6, 9.7) (main, casl, oof)
##
## with a 52 week window and the `part_` epi trackers
##
##
library("methods")
nsims <- ncores <- 3
lnt <- TRUE
source("R/utils-params.R")
pull_env_vars()
param$part.ident.start <- 5
control <- control_msm(
  simno = 1,
  nsteps = 52 * 60,
  nsims = ncores,
  ncores = ncores,
  save.nwstats = TRUE,
  # raw.output = TRUE,
  raw.output = FALSE,
  verbose = FALSE
)

## Simulation
sim <- netsim(orig, param, init, control)

library(dplyr)

df <- as.data.frame(sim)

df %>%
  filter(time > max(time - 520)) %>%
  select(starts_with("part_")) %>%
  summarize(across(
    everything(),
    list(
      m = ~ mean(.x, na.rm = T),
      s = ~ sd(.x, na.rm = T)
    )
  ))

x <- c(1.1, 1.6, 9.7)

## option 1 - we enforce part.indent.main.prob == part.indent.casl.prob == 2 * part.indent.oof.prob
p_temp <- 0.432 / sum(x * c(1, 1, 1/2))
p_temp
# [1] 0.09653631
p <- c(p_temp, p_temp, p_temp / 2)
p
# [1] 0.09653631 0.09653631 0.04826816# number of partners identified by type
p * x
# [1] 0.1003978 0.1428737 0.1887285# validation
sum(p * x)
# [1] 0.432

## option 2
##
## we force each partnership type to give the same "number" of partners
p <- 0.432 / 3 / x
p
# [1] 0.13846154 0.09729730 0.03682864# number of partners identified by type
p * x
# [1] 0.144 0.144 0.144# validation
sum(p * x)
# [1] 0.432
