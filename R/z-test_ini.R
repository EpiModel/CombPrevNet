source("R/utils-params.R", local = TRUE)

nsteps <- 52 * 10

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

param$tx.halt.partial.prob <- c(0.0057, 0.0047, 0.0028)
param$epi_trackers <- restart_trackers

sim <- netsim(orig, param, init, control)
savesim(sim, save.min = FALSE, save.max = TRUE, compress = TRUE, time.stamp = FALSE)

library(tidyverse)

df_b <- as_tibble(sim)

df_t <- df_b %>%
    group_by(sim, time) %>%
    mutate(
      ir100.gc = median(ir100.gc, na.rm = TRUE),
      ir100.ct = median(ir100.ct, na.rm = TRUE),
      i.prev.dx.B = median(i_dx___B / n___B, na.rm = TRUE),
      i.prev.dx.H = median(i_dx___H / n___H, na.rm = TRUE),
      i.prev.dx.W = median(i_dx___W / n___W, na.rm = TRUE),
      cc.dx.B = median(i_dx___B / i___B, na.rm = TRUE),
      cc.dx.H = median(i_dx___H / i___H, na.rm = TRUE),
      cc.dx.W = median(i_dx___W / i___W, na.rm = TRUE),
      cc.linked1m.B = median(linked1m___B / i_dx___B, na.rm = TRUE),
      cc.linked1m.H = median(linked1m___H / i_dx___H, na.rm = TRUE),
      cc.linked1m.W = median(linked1m___W / i_dx___W, na.rm = TRUE),
      cc.vsupp.B = median(i_sup___B / i_dx___B, na.rm = TRUE),
      cc.vsupp.H = median(i_sup___H / i_dx___H, na.rm = TRUE),
      cc.vsupp.W = median(i_sup___W / i_dx___W, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(
      time,
      i.prev.dx.B, i.prev.dx.H, i.prev.dx.W,
      cc.dx.B, cc.dx.H, cc.dx.W,
      cc.linked1m.B, cc.linked1m.H, cc.linked1m.W,
      cc.vsupp.B, cc.vsupp.H, cc.vsupp.W,
      ir100.gc, ir100.ct
    )

df_t %>%
  filter(time > max(time) - 52) %>%
  summarise(across(-time, ~ median(.x, na.rm = TRUE))) %>%
  as.list()



