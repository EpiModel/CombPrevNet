library("methods")
# to use devl EpiModelHIV change in "R/utils-params.R"x

nsims <- ncores <- 1

lnt <- TRUE
source("R/utils-params.R")
pull_env_vars()

control <- control_msm(
  simno = 1,
  nsteps = 52 * 4,
  nsims = ncores,
  ncores = ncores,
  save.nwstats = TRUE,
  raw.output = TRUE,
  verbose = TRUE
)

param$part.ident.start <- 5

## Simulation
sim <- netsim(orig, param, init, control)

# Tests
if (control$raw.output) {
  dat <- sim[[1]]
} else {
  df <- as.data.frame(sim)
  df <- df[df$time > max(df$time) - 10, ]
}
