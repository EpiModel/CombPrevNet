## The observed number of identified partners in Atlanta is 0.432 and
## the observed number of partners in my simulations is
## x <- c(1.1, 1.6, 9.7) (main, casl, oof)
##
## with a 52 week window and the `epi_partner_count` epi tracker
##
##
library("methods")
nsims <- ncores <- 3
lnt <- TRUE
source("R/utils-params.R")
pull_env_vars()
param$part.ident.start <- 5
param$epi_trackers <- list("partner_count" = epi_partner_count)
control <- control_msm(
  simno = 1,
  nsteps = 52 * 25,
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
  filter(time > 52) %>%
  select(partner_count) %>%
  mutate(
    p1 = partner_count %/% 1e3^0 %% 1e3,
    p2 = partner_count %/% 1e3^1 %% 1e3,
    p3 = partner_count %/% 1e3^2 %% 1e3
  ) %>%
  summarize(across(
    c(p1, p2, p3),
    list(
      m = ~ mean(.x, na.rm = T),
      s = ~ sd(.x, na.rm = T)
    )
  ))


x <- c(1.6, 2.5, 15.75)
target_ident <- 0.432

## option 1 - we enforce part.indent.main.prob == part.indent.casl.prob == 2 * part.indent.oof.prob
p_temp <- target_ident / sum(x * c(1, 1, 1/2))
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
p <- target_ident / 3 / x
p
# [1] 0.13846154 0.09729730 0.03682864# number of partners identified by type
p * x
# [1] 0.144 0.144 0.144# validation
sum(p * x)
# [1] 0.432
