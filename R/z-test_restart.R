source("R/utils-params.R", local = TRUE)
pkgload::load_all("../EpiModelHIV-p")
orig <- readRDS("out/restart_test.rds")

window_size <- 52

nsteps <- 52 * 20
param$prep.risk.int <- 52
param$part.index.prob <- 1
param$part.ident.main.prob <- 1
param$part.ident.casl.prob <- 1
param$part.ident.ooff.prob <- 1
param$part.hiv.test.rate   <- rep(1, 3)
param$part.prep.start.prob <- rep(1, 3)
param$part.tx.init.prob    <- rep(1, 3)
param$part.tx.reinit.prob  <- rep(1, 3)

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
sim_sav <- sim

library(tidyverse)

lapply(sim$epi, \(x) length(x[[1]]))
sim$epi <- Filter(\(x) length(x[[1]]) == 4213, sim$epi)
df <- as_tibble(sim)

names(df)

df %>%
  filter(time > max(time) - 52 * 10) %>%
  summarise(
    part_sneg___ALL = sum(part_sneg___ALL, na.rm = TRUE),
    part_prep___ALL = sum(part_prep___ALL, na.rm = TRUE)
  )

df %>%
  filter(time > max(time) - 104) %>%
  summarise(
    coverage = median(s_prep___ALL / s_prep_elig___ALL),
    time_on = median(prep_time_on___ALL),
    retention_1y = median(prep_1y___ALL),
    found_indexes = sum(found_indexes, na.rm = TRUE),
    elig_partners = sum(elig_partners, na.rm = TRUE),
    found_partners = sum(found_partners, na.rm = TRUE)
  ) |> print()


df %>%
  ggplot(aes(
    x = time/52, y = s_prep___ALL / s_prep_elig___ALL)) +
  geom_smooth()
