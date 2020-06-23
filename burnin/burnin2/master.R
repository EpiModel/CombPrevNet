
## build master.sh script ##

library("EpiModelHPC")

# PrEP Prevalence Calibration ---------------------------------------------

# Calibrate parameter
vars <- NULL
sbatch_master(vars = vars,
              master.file = "burnin/burnin2/master.sh",
              runsim.file = "runsim.sh",
              simno.start = 300,
              ckpt = TRUE,
              nsims = 250,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# Pick best fitting sim
sbatch_master(vars = vars,
              master.file = "burnin/burnin2/master.sh",
              runsim.file = "runsim.sh",
              simno.start = 400,
              ckpt = TRUE,
              nsims = 250,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
