
# 09-test_netsim.R
#
# This file runs the model from timestep 1 using the inputs defined in
# "R/utils-inputs.R"

# > gc()
#           used (Mb) gc trigger (Mb) max used (Mb)
# Ncells  454612 24.3     953347   51   691088 37.0
# Vcells 1526549 11.7    8388608   64  4608561 35.2

source("R/utils-inputs.R", local = TRUE)

nsim <- 1
control <- control_msm(
  nsteps = 52 * 10,
  nsims = nsim,
  ncores = nsim,
  truncate.el.cuml = 53,
  tracker.list = epi_trackers,
  raw.output = FALSE,
  verbose = FALSE
)

# > gc()
           # used  (Mb) gc trigger  (Mb) max used  (Mb)
# Ncells  5281792 282.1    8577165 458.1  5706747 304.8
# Vcells 33991052 259.4   48885699 373.0 34348561 262.1

# > rm(list = ls())
# > gc()
#           used  (Mb) gc trigger  (Mb) max used  (Mb)
# Ncells 2471377 132.0    6861732 366.5  5706747 304.8
# Vcells 6088415  46.5   39108560 298.4 34348561 262.1

sim <- netsim(orig, param, init, control)

# gc()
#            used  (Mb) gc trigger  (Mb) max used  (Mb)
# Ncells  5375842 287.2   10511635 561.4 10511635 561.4
# Vcells 35262974 269.1   63171618 482.0 63171618 482.0

# > rm(list = ls())
# > gc()
          # used  (Mb) gc trigger  (Mb) max used  (Mb)
# Ncells 2565156 137.0    8409308 449.2 10511635 561.4
# Vcells 6354347  48.5   50537295 385.6 63171618 482.0


# sim$epi <- sim$epi["num"] # keep only the "num" epi tracker
# saveRDS(sim, "out/est/restart.rds")

