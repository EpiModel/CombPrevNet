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
  df_ls <- vector(mode = "list", length = length(sim_files))
  btch <- 0
  for (fle in sim_files) {
    btch <- btch + 1
    sim <- readRDS(fle)
    dff <- as.data.table(sim)
    dff[, `:=`(batch = btch, param_batch = infos$unique_proposals[btch])]
    dff <- dff[,
      .SD, .SDcols = c("param_batch", "batch", "sim", "time", names(targets))
    ]
    # do some transforms here (or not but risk memory overflow)
    #
    df_ls[[btch]] <- dff
  }
  jobs[[job]]$data <- rbindlist(df_ls)
}

df <- jobs[[1]]$data
proposals <- jobs[[1]]$infos$param_proposals[1:max(df$param_batch)]

df <- df[time >= max(time) - 52, ]
df[, lapply(.SD, median), .SDcols = names(targets), by = "param_batch"]

#as.list(df[, lapply(.SD, median), .SDcols = names(targets)])

# match param_proposal to file at some point (in `infos`)
