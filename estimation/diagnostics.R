
##
## Diagnostics for Combination Prevention Model
##

## Packages ##
rm(list = ls())
suppressMessages(library("EpiModelHIV"))

est <- readRDS("est/netest.rda")
netstats <- readRDS("est/netstats.rda")


# Main --------------------------------------------------------------------

fit_main <- est[[1]]

model_main_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", base = 0) +
  nodefactor("deg.casl", base = 0) +
  degrange(from = 3) +
  concurrent +
  nodematch("role.class", diff = TRUE) +
  degree(0:3)
dx_main <- netdx(fit_main, nsims = 10, ncores = 6, nsteps = 500,
                 nwstats.formula = model_main_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_main)
plot(dx_main)

netstats$main


# Casual ------------------------------------------------------------------

fit_casl <- est[[2]]

model_casl_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", base = 0) +
  nodefactor("deg.main", base = 0) +
  degrange(from = 4) +
  concurrent +
  nodematch("role.class", diff = TRUE) +
  degree(0:4)
dx_casl <- netdx(fit_casl, nsims = 10, ncores = 6, nsteps = 500,
                 nwstats.formula = model_casl_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))
print(dx_casl, digits = 1)
plot(dx_casl)

netstats$casl


# One-Off -----------------------------------------------------------------

fit_inst <- est[[3]]

model_inst_dx <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", base = 0) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", base = 0) +
  nodefactor("risk.grp", base = 0) +
  nodefactor("deg.tot", base = 0) +
  nodematch("role.class", diff = TRUE) +
  degree(0:4)
dx_inst <- netdx(fit_inst, nsims = 10000, dynamic = FALSE,
                 nwstats.formula = model_inst_dx,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5))

print(dx_inst, digits = 1)

plot(dx_inst, sim.lines = TRUE, sim.lwd = 0.05)

netstats$inst
