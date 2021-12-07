# define the `scenarios_df` data frame.
# here is a small example with `tribble` but loading a csv or excel file could
# be popular solution
scenarios_df <- tibble::tribble(
  ~vl.acute.fall, ~hiv.test.rate_1, ~hiv.test.rate_2, ~hiv.test.rate_3,
  3,              0.00385,          0.00385,          0.0069,
  4,              0.004,            0.004,            0.008,
  5,              0.005,            0.005,            0.009
)

# define how many cpu there is per node and how many replication of each
# scenarios are necessary
n_cpus <- 28
n_replications <- 500

# this function will initiate a "workflow".
# A workflow can have multiple steps to be run (sequencially in this case)
# We provide the "common" slurm args here (account and partition)
workflow <- swf_new_workflow(
  "test_workflow",
  slurm_config = list(
    partition = "ckpt",
    account = "csde-ckpt"
  )
)

# adding a step to a workflow correspond to adding a job to it.
# It needs:
#   - the object returned by `swf_create_workflow`
#   - a script to be run (file paths are relative to the root of the project)
#   - a set of parameters to pass to sbatch
#     in this case, we want 500 repetitions of each scenario. Because we have 28
#     cpu per node we will run each scenario 18 * 28 = 504 times. Because we
#     have 3 different scenarios, we get an array of 18 * 3 = 54.
#     We also ask to receive a mail if the job fails but not upon normal
#     completion
workflow <- swf_add_step(
  workflow,
  script = "R/swf-scenario_run.R",
  slurm_config = list(
    "array"        = nrow(scenarios_df) * (n_replications %/% n_cpus + 1),
    "cpu-per-task" = n_cpus,
    "mem-per-cpu"  = 5 * 1e3,
    "walltime"     = 60,
    "mail"         = "FAIL"
  )
)

# script providing a `create_model_inputs` function creating an `inputs` list
#  containing `est`, `param`, `init`, `control`
sources("R/utils-inputs.R")
inputs <- create_model_inputs(ncores = n_cpus)

# We may want to have object available to the workflow without saving them in
# the main project folder
# `swf_get_root(workflow)` returns a path to the workflow folder. With it we can
# save files inside this folder to avoid cluttering the project or creating name
# collisions
swf_root <- swf_get_root(workflow)

# Here we use the `fs` package to create clean paths consistently between
# Operating Systems
# the "inputs.rds" file will live in the "in" subfolder of the workflow
# directory.
saveRDS(inputs, fs::path(swf_root, "in/inputs.rds"))

# Finally we add a second step to our workflow.
# This one will process the scenarios once the first step is done
# Notice that in this case we do not specify the `array` argument as it should
# be run only once.
# Further, me ask to receive a mail upon any for of completion. This way we can
# let the workflow run until an email tells us it is ready to be downloaded.
workflow <- swf_add_step(
  workflow,
  script = "R/swf-scenario_process.R",
  slurm_config = list(
    "cpu-per-task" = 28,
    "mem-per-cpu"  = 5 * 1e3,
    "walltime"     = 45,
    "mail"         = "END"
  )
)

# Testing the functions locally
#
# we set the environment variable to mimic what will be used on slurm
# for the first step, we mimic the 10th array element and 2 CPUs
Sys.setenv("SLURM_ARRAY_TASK_ID", "10")
Sys.setenv("SLURM_CPUS_PER_TASK", "2")
swf_set_root_var(workflow)

source("R/swf-scenario_run.R")

# for the second step, the ARRAY_ID is irrelevant and we still use 2 cpus 2 CPUs
Sys.setenv("SLURM_CPUS_PER_TASK", "2")
source("R/swf-scenario_process.R")

# finally we unset the variables
Sys.setenv("SLURM_ARRAY_TASK_ID", "")
Sys.setenv("SLURM_CPUS_PER_TASK", "")
swf_set_root_var("")

# this approach allows a local debugging before going to slurm
