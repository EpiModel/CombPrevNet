
## build master.sh script ##

library("EpiModelHPC")

# Test stability of run
vars <- NULL
sbatch_master(vars = vars,
              master.file = "burnin/burnin1/master.sh",
              simno.start = 100,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# Big run batch for model selection
sbatch_master(vars = vars,
              master.file = "burnin/burnin1/master.sh",
              simno.start = 200,
              ckpt = TRUE,
              nsims = 10000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")
