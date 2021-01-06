library(data.table)

# One or many job_names
job_names <- c()
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n)) {
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)
}

jobs <- list()

# Read targets
source("R/utils-targets.R")

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos

  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  jobs[[job]]$data <- data.table()
  btch <- 0
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)
    dff <- if (is.null(infos$df_keep)) as.data.table(sim) else sim
    dff[, batch := btch]
    dff <- dff[, .SD, .SDcols = c("batch", "sim", "time", names(targets))]
    # do some transforms here (or not but risk memory overflow)
    #

    jobs[[job]]$data <- rbind(jobs[[job]]$data, dff)
  }
}

df <- jobs[[1]]$data

as.list(df[, lapply(.SD, max, na.rm = T), .SDcols = names(targets)])


sim_folders <- paste0("out/param_calibration/", job_names)
sim_files <- fs::dir_ls(sim_folders, recurse = TRUE,
                        regexp = "df_sim\\d*.rds")

param_proposals <- lapply(sim_folders, function(folder) {
  readRDS(paste0(folder, "/params.R"))

})

df <- data.table()
i <- 0

for (f_name in sim_files) {
  i <- i + 1

  df <- readRDS(f_name)
  setDT(df)
  df[, file_name := f_name]
  dt <- rbind(dt, df)
}
