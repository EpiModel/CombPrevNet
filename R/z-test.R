library("methods")
# to use devl EpiModelHIV change in "R/utils-params.R"x

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

# Tests
if (control$raw.output) {
  dat <- sim[[1]]
} else {
  df <- as.data.frame(sim)
  df <- df[df$time > max(df$time) - 10, ]
}


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
