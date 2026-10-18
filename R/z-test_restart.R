# This pulls in the default `param` object and the trackers
source("R/utils-params.R", local = TRUE)
# pkgload::load_all("../EpiModelHIV-p")
orig <- readRDS("out/est/restart.rds")

nsteps <- 52 * 20

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 28,
  ncores = 7,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

main <- 0.645
param$part.ident.main.prob <- main
param$part.ident.casl.prob <- plogis(qlogis(main) - log(2))
param$part.ident.ooff.prob <- plogis(qlogis(main) - log(4))

sim <- netsim(orig, param, init, control)

library(tidyverse)
d <- as_tibble(sim)

d %>%
  filter(
    time > max(time) - 52 * 6,
  ) %>%
  mutate(
    elic = found_partners / found_indexes,
    found = found_partners / elig_partners
  ) %>%
  summarise(
    elic_mean = mean(elic, na.rm = TRUE),
    elic_sd = sd(elic, na.rm = TRUE),
    found_mean = mean(found, na.rm = TRUE),
    found_sd = sd(found, na.rm = TRUE)
  ) %>%
  print(n = 200)

