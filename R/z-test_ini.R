lnt <- TRUE
source("R/utils-params.R", local = TRUE)

window_size <- 52

prep_start_time <- 52 * 61 + 1

nsteps <- 52 * 85

control <- control_msm(
  nsteps =  nsteps, # one year for prep riskhist then nsteps
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  # initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

options(error = recover)
# debug(stitrans_msm)
sim <- netsim(orig, param, init, control)
# saveRDS(sim, "out/restart_test.rds")

library(tidyverse)

df <- as_tibble(sim)

df %>%
  # mutate(y = s_prep___ALL / (s_prep_elig___ALL)) %>%
   mutate(y = ir100.ct) %>%
ggplot(aes(x = time, y = y)) +
  geom_line()
