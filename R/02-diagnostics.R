
##
## 02. Network Model Diagnostics
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

est <- readRDS("data/input/netest.rds")
netstats <- readRDS("data/input/netstats.rds")

ncores <- parallel::detectCores() / 2
nsims <- ncores * 1

# Main --------------------------------------------------------------------

fit_main <- est[[1]]

model_main_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = TRUE) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = TRUE) +
  nodefactor("deg.casl", levels = TRUE) +
  degrange(from = 3) +
  concurrent +
  nodematch("role.class", diff = TRUE) +
  degree(0:3)
dx_main <- netdx(fit_main, nsims = nsims, ncores = ncores, nsteps = 500,
                 nwstats.formula = model_main_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_main, digits = 1)

netstats$main


# Casual ------------------------------------------------------------------

fit_casl <- est[[2]]

model_casl_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = TRUE) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = TRUE) +
  nodefactor("deg.main", levels = TRUE) +
  degrange(from = 4) +
  concurrent +
  nodematch("role.class", diff = TRUE) +
  degree(0:4)
dx_casl <- netdx(fit_casl, nsims = nsims, ncores = ncores, nsteps = 500,
                 nwstats.formula = model_casl_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl, digits = 1)

netstats$casl


# One-Off -----------------------------------------------------------------

fit_inst <- est[[3]]

model_inst_dx <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", levels = TRUE) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = TRUE) +
  nodefactor("risk.grp", levels = TRUE) +
  nodefactor("deg.tot", levels = TRUE) +
  nodematch("role.class", diff = TRUE) +
  degree(0:4)
dx_inst <- netdx(fit_inst, nsims = 10000, dynamic = FALSE,
                 nwstats.formula = model_inst_dx,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))

print(dx_inst, digits = 1)

netstats$inst
