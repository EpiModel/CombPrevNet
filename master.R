
## build master.sh script ##

library("EpiModelHPC")

# Test stability of run
vars <- NULL
sbatch_master(vars = vars,
              master.file = "master-burnin1.sh",
              env.file = "loadR.sh",
              rscript.file = "R/03-burnin1-simMin.R",
              build.runsim = TRUE,
              simno.start = 1000,
              ckpt = FALSE,
              nsims = 35,
              ncores = 35,
              walltime = "00:30:00",
              mem = "120G")
