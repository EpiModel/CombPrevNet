source("R/utils-params.R", local = TRUE)
# pkgload::load_all("../EpiModelHIV-p")
orig <- readRDS("out/est/restart.rds")

nsteps <- 52 * 20

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 4,
  ncores = 4,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

# param$part.ident.main.prob <- 0.165
# param$part.ident.casl.prob <- param$part.ident.main.prob / 2
# param$part.ident.ooff.prob <- param$part.ident.main.prob / 4


# debug(hivtest_msm)
sim <- netsim(orig, param, init, control)
sim_sav <- sim

library(tidyverse)

df <- as_tibble(sim)

names(df)


df %>%
  filter(
    time > max(time) - 52 * 10,
  ) %>%
  summarise(
    found = sum(found_partners, na.rm = T) / sum(found_indexes, na.rm = T)
  ) %>% print()

df %>%
  filter(time > max(time) - 52 * 10) %>%
  summarise(
    coverage = median(s_prep___ALL / s_prep_elig___ALL),
    time_on = median(prep_time_on___ALL),
    retention_1y = median(prep_1y___ALL)
  ) %>% print()


df %>%
  filter(time > max(time) - 52 * 1) %>%
  summarise(
    coverage = median(s_prep___ALL / s_prep_elig___ALL),
    time_on = median(prep_time_on___ALL),
    retention_1y = median(prep_1y___ALL)
  ) %>% print()
