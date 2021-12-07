swf_get_root <- function(workflow = NULL) {
  if (is.null(workflow)) {
    swf_root <- Sys.getenv("SWF_ROOT")
  } else {
    swf_root <- workflow[["root"]]
  }

  swf_root
}

swf_set_root_var <- function(swf_root) {
  Sys.setenv("SWF_ROOT", swf_root)
}

swf_new_workflow <- function(workflow_name) {
  # Copy swf_template folder

  list(
    name = workflow_name,
    root = "path/to/swf/root",
    last_step = 0
  )
}

swf_get_workflow <- function(workflow_name, dir = NULL) {
  # get workfow "workflow_name" in "dir" (default workfol dir)
}

swf_add_step <- function(workflow, script, slurm_config) {

  workflow[["last_step"]] <- workflow[["last_step"]] + 1
  workflow
}


