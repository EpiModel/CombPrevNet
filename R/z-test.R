lnt <- TRUE
source("R/utils-params.R", local = TRUE)
orig <- readRDS("out/restart_test.rds")

window_size <- 52

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

 param$prep.start.prob <-  rep(0.71, 3) # 0.00896,
 param$prep.discont.rate <- rep(0.0087, 3) # 1 - (2^(-1/(224.4237/7)))
 param$prep.risk.int <- 26
 param$prep.risk.reassess.method <- "year"
 param$prep.require.lnt <- TRUE


# debug(hivtest_msm)
sim <- netsim(orig, param, init, control)

library(tidyverse)
theme_set(theme_light())

df <- as_tibble(sim)

# df %>%
#   mutate(
#     prev = s_prep___ALL / s_prep_elig___ALL,
#     on = prep_time_on___ALL,
#     year = prep_1y___ALL
#   ) %>%
#   ggplot(aes(x = time / 52, y = year)) +
#     geom_smooth()

df %>%
  filter(time > max(time) - 104) %>%
  summarise(
    coverage = median(s_prep___ALL / s_prep_elig___ALL),
    time_on = median(prep_time_on___ALL),
    retention_1y = median(prep_1y___ALL)
  ) |> print()

