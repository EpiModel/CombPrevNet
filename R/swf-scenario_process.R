# source the file containing the simulation processing function
# note that the path here is relative to the project working directory
source("R/utils-outcomes.R")

# get the Slurm WorkFlow root (path to folder)
swf_root <-swf_get_root()
# get the number of CPU used for this task. it is accessed throuth the
# "SLURM_CPUS_PER_TASK" environment variable created by sbatch
n_cpus <- Sys.getenv("SLURM_CPUS_PER_TASK")

# list all netsim files in "out/"
filenames <- fs::dir_ls(fs::path(swf_root, "out"), regexp = "sim_.*\\.rds")

# process each netsim file in parallel with `future_lapply` and the
# `make_outcomes` function from the "R/utils-outcomes.R" script
future::plan(future::multicore, workers = n_cpus)
sim_ls <- future.apply::future_lapply(filenames, make_outcomes)
# bind all the data frames
sim_df <- dplyr::bind_rows(tibble_ls)

# save the fine in the "out" subfolder inside the workflow
saveRDS(sim_df, fs::path(swf_root, "out/outcomes.R"), compress = "xz")
# this time the file is compressed with 'xz', it's space efficient but pretty
# slow. But this file is meant to be downloaded from the HPC so reducing its
# size makes sense.

