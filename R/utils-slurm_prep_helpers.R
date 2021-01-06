seq_cross <- function(lows, highs, length.out, cross = FALSE) {
  lsts <- purrr::map2(lows, highs, ~ seq(.x, .y, length.out = length.out))
  c_lsts <- purrr::cross(lsts)
  purrr::map(c_lsts, purrr::flatten_dbl)
}

transpose_ragged <- function(x) {
  lgts <- max(purrr::map_dbl(x, length))
  out <- purrr::map(seq_len(lgts), ~ list())

  for (i in seq_len(lgts)) {
    for (nms in names(x)) {
      if (length(x[[nms]]) >= i)
        out[[i]][[nms]] <- x[[nms]][[i]]
    }
  }

  out
}

make_relative_params <- function(param_proposals, relative_params) {
  lapply(param_proposals, function(param) {
    for (nms in names(relative_params))
      param[[nms]] <- relative_params[[nms]](param)
    param
  })
}

run_netsim_fun <- function(param_proposal, sim_num,
                           orig, param, init, control, info) {
  library(EpiModelHIV)

  # Helper function
  update_list <- function(x, new_x) {
    for (n in names(new_x)) {
      if (is.list(new_x[[n]])) {
        x[[n]] <- update_list(x[[n]], new_x[[n]])
      } else if (is.function(new_x[[n]]) && ! is.function(x[[n]])) {
        x[[n]] <- new_x[[n]](x[[n]])
      } else {
        x[[n]] <- new_x[[n]]
      }
    }
    return(x)
  }

  param <- update_list(param, param_proposal)
  sim <- netsim(orig, param, init, control)

  prefix <- ""
  if (!is.null(info$df_keep)){
    prefix <- "df_"
    df <- as.data.frame(sim)
    df <- df[df$time > max(df$time) - info$df_keep, ]
    saveRDS(df, paste0(info$root_dir, "/out/", prefix, "sim", sim_num, ".rds"))
  } else {
    saveRDS(sim, paste0(info$root_dir, "/out/", prefix, "sim", sim_num, ".rds"))
  }
}

run_netsim_updaters_fun <- function(updaters, sim_num, scenario,
                           orig, param, init, control, info) {
  library(EpiModelHIV)

  param$param_updaters <- c(param$param_updaters, updaters)
  sim <- netsim(orig, param, init, control)

  prefix <- ""
  if (!is.null(info$df_keep)){
    prefix <- "df_"
    df <- as.data.frame(sim)
    df <- df[df$time > max(df$time) - info$df_keep, ]
    df$scenario <- scenario
    saveRDS(df, paste0(info$root_dir, "/out/", prefix, "sim", sim_num, ".rds"))
  } else {
    saveRDS(sim, paste0(info$root_dir, "/out/", prefix, "sim", sim_num, ".rds"))
  }


}
make_job_paths <- function(job_name, ssh_dir, ssh_host) {
  p <- list()

  p$ssh_proj <- ssh_dir
  p$local_out <- fs::path("out/")
  p$jobs_dir <- fs::path("remote_jobs/")
  p$local_job_dir <- fs::path(p$local_out, p$jobs_dir, job_name)
  p$remote_job_dir <- fs::path(p$jobs_dir, job_name)
  p$ssh_job_dir <- fs::path(ssh_dir, p$jobs_dir, job_name)
  p$slurm_wf <- fs::path("slurm/")
  p$slurm_out <- fs::path(p$slurm_wf, "out")

  p
}
