library(EpiModelHIV)

# get the Slurm WorkFlow root (path to folder)
# note that we do not provide a `workflow` object here as EpiModelHPC will infer
# that from the Slurm environment
swf_root <- swf_get_root()

# read in variables previously stored inside the Slurm WorkFlow folder
inputs <- readRDS(fs::path(swf_root, "in/inputs.rds"))

# read the scenarios data frame
scenarios_df <- readRDS(fs::path(swf_root, "in/scenarios_df.rds"))

# chose a scenario using the task id (1:array_size)
# it is accessed throuth the "SLURM_ARRAY_TASK_ID" environment variable created
# by sbatch
array_id    <- Sys.getenv("SLURM_ARRAY_TASK_ID")
scenarios_n <- nrow(scenarios_df)
scenario_no <- array_id %% scenarios_n + 1 # + 1 as R count starting from 1

# update the parameters according to the scenario
param <- update_params(inputs$param, scenarios_df[scenario_no, ])

# get the number of CPU used for this task. it is accessed throuth the
# "SLURM_CPUS_PER_TASK" environment variable created by sbatch
n_cpus <- Sys.getenv("SLURM_CPUS_PER_TASK")
control <- inputs$control
control$nsims <- n_cpus
control$ncores <- n_cpus

# run the simulation
sim <- netsim(inputs$est, param, inputs$init, control)

# store the netsim object with a parsable name inside the "out" subfolder inside
# the workflow
file_name <- paste0("out/sim_", scenario_no, "_", array_id, ".rds")
saveRDS(sim, fs::path(swf_root, file_name), compress = FALSE)
# the file is not compressed here as it is meant to be processed by the HPC
# during the next step. So we prioritize speed over space efficiency.

