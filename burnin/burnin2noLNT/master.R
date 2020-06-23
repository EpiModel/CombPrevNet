
## build master.sh script ##

library("EpiModelHPC")

# PrEP Prevalence Calibration ---------------------------------------------

vars <- NULL
sbatch_master(vars = vars,
              master.file = "burnin/burnin2noLNT/master.sh",
              runsim.file = "runsim.sh",
              simno.start = 500,
              ckpt = TRUE,
              nsims = 250,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

sbatch_master(vars = NULL,
              master.file = "burnin/burnin2noLNT/master.sh",
              runsim.file = "runsim.sh",
              simno.start = 600,
              ckpt = TRUE,
              nsims = 250,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
