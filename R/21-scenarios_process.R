library(data.table)

# One or many job_names
job_names <- "CPN_sc_9"
job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n))
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)

# Read targets
source("R/utils-targets.R")

needed_cols <- c(
  "sim", "time", "batch", "scenario",
  "incid", "ir100",
  "s_prep___ALL", "s_prep_elig___ALL",
  "i___ALL", "i_dx___ALL", "i_tx___ALL", "i_sup___ALL",
  "elig_indexes", "found_indexes", "prep.start.counter",
  "prep_start___ALL", "prep_time_on___ALL", "prep_episodes___ALL",
  "part_ident___ALL", "part_sneg___ALL", "part_spos___ALL",
  "part_prep___ALL", "part_txinit___ALL", "part_txreinit___ALL",
  "ident_dist0___ALL", "ident_dist1___ALL",
  "ident_dist2___ALL", "ident_dist3p___ALL"
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
    dff[, `:=`(batch = btch, scenario = names(infos$param_proposals)[btch])]
    keep_cols <- intersect(needed_cols, names(dff))
    df_ls[[btch]] <- dff[, ..keep_cols]
  }
  jobs[[job]]$data <- rbindlist(df_ls, fill = TRUE)
}

df <- map_dfr(jobs, ~ as_tibble(.x$data))
saveRDS(df, "out/scdf.rds")
