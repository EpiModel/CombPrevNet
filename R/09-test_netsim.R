
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"
#
source("R/utils-inputs.R", local = TRUE)

control <- control_msm(
  nsteps = 52 * 65,
  nsims = 1,
  ncores = 1,
  truncate.el.cuml = 53,
  tracker.list = epi_trackers,
  verbose = FALSE,
  raw.output = FALSE
)

# sim <- netsim(orig, param, init, control)

options(browser = "firefox")
profvis::profvis({
  sim <- netsim(orig, param, init, control)
})

# sim$epi <- sim$epi["num"] # keep only the "num" epi tracker
# saveRDS(sim, "out/est/restart.rds")

