scenarios_df <- tibble::tribble(
  ~vl.acute.fall, ~hiv.test.rate_1, ~hiv.test.rate_2, ~hiv.test.rate_3,
  3,              0.00385,          0.00385,          0.0069,
  4,              0.004,            0.004,            0.008,
  5,              0.005,            0.005,            0.009
)

n_cpus <- 28
n_replications <- 500

workflow <- swf_new_workflow(
  "test_workflow",
  slurm_config = list(
    partition = "ckpt",
    account = "csde-ckpt"
  )
)

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
