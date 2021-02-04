library("methods")
# to use devl EpiModelHIV change in "R/utils-params.R"x

nsims <- ncores <- 1

lnt <- TRUE
source("R/utils-params.R")
pull_env_vars()

param$prep.start <- (52 * 1) + 1
param$riskh.start <- param$prep.start - 52
# param$prep.start.prob <-  c(0, 0, 0)
# param$part.prep.start.prob <- c(0, 0, 0.5)
param$prep.discont.rate <- c(0, 0, 0.99)

param$part.ident.start <- Inf

control <- control_msm(
  simno = 1,
  nsteps = 52 * 4,
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

df %>% select(starts_with("s_prep")) %>% print()
