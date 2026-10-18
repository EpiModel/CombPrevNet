library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(EpiModel)

# One or many job_names
job_names <- "CPN_ident"
# job_names <- "CPN_restart_tmp"
job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n))
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)

# Read targets
source("R/utils-targets.R")

needed_trackers <- c(
  "n",
  "i", "i_dx", "i_sup", "linked1m",
  "s", "s_prep", "s_prep_elig",
  "prep_time_on", "prep_1y",
  "gc_s", "ct_s", "gc_i", "ct_i",
  "part_ident"
)

needed_pops <- c("ALL", "B", "H", "W")

needed_trackers <- vapply(
  needed_pops,
  function(pop) paste0(needed_trackers, "___", pop),
  needed_trackers
)

needed_cols <- c(
  "sim", "time", "batch", "param_batch", "num",
  "incid", "ir100.gc", "ir100.ct",
  "prepStartPart",
  "found_indexes", "elig_indexes",
  "found_partners", "elig_partners",
  needed_trackers
)

jobs <- list()
for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos
  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  df_ls <- vector(mode = "list", length = length(sim_files))
  for (fle in sim_files) {
    btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
    sim <- readRDS(fle)
    dff <- as.data.table(sim)
        dff <- dff[time > max(time) - 52 * 20] ###########################################################
    dff[, `:=`(batch = btch, param_batch = infos$unique_proposals[btch])]
    keep_cols <- intersect(needed_cols, names(dff))
    df_ls[[btch]] <- dff[, ..keep_cols]
  }
  df_ls <- discard(df_ls, ~ is.null(.x))
  jobs[[job]]$data <- rbindlist(df_ls, fill = TRUE)
}

saveRDS(jobs, "out/calib_jobs.rds")
