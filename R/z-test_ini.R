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

# options(error = recover)
# debug(stitrans_msm)
sim <- netsim(orig, param, init, control)
# saveRDS(sim, "out/restart_test.rds")

library(tidyverse)

df <- as_tibble(sim)

df %>%
  filter(time > 52 * 65) %>%
  mutate(y = s_prep___ALL / (s_prep_elig___ALL)) %>%
ggplot(aes(x = time, y = y)) +
  geom_line()

df %>%
  filter(between(time, 52 * 70, 52 * 71)) %>%
  summarise(
    prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE)
  ) |> print(n = 200)
