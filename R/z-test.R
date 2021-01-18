library("methods")
pkgload::load_all("../EpiModelHIV-p")

## Environmental Arguments
pull_env_vars()

nsims <- ncores <- 1

lnt <- TRUE
source("R/utils-params.R")
## Parameters

# pkgload::load_all("~/git/EpiModelHIV-p")
control <- control_msm(
  simno = 1,
  nsteps = 52 * 60,
  nsims = ncores,
  ncores = ncores,
  save.nwstats = TRUE,
  # raw.output = TRUE,
  verbose = TRUE
)

## Simulation
sim <- netsim(orig, param, init, control)

df <- as.data.frame(sim)
df <- df[df$time > 52 * 60 - 10,]
