library(data.table)

# One or many job_names
job_names <- c("CPN_new_EMH")
job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names
keep_all_epi <- TRUE # set to false to keep only the "targets"

if (!is.null(job_last_n))
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)

# Read targets
source("R/utils-targets.R")

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
    dff[, `:=`(batch = btch, param_batch = infos$unique_proposals[btch])]
    # Keep only the "targets"
    if (! keep_all_epi) {
      dff <- dff[,
        .SD, .SDcols = c("param_batch", "batch", "sim", "time", names(targets))]
    }
    # do some transforms here (or not but risk memory overflow)
    #
    df_ls[[btch]] <- dff
  }
  jobs[[job]]$data <- rbindlist(df_ls)
}

library(tidyverse)

df <- as_tibble(jobs[[1]]$data)
proposals <- jobs[[1]]$infos$param_proposals[1:max(df$param_batch)]

# df <- filter(df, time >= max(time) - 52)

source("R/utils-targets.R")
prevs <- targets[startsWith(names(targets), "i.prev")]

m <- 0.003
lst <- df %>%
  group_by(param_batch) %>%
  summarise(across(c(i.prev.dx.B, i.prev.dx.H, i.prev.dx.W), median)) %>%
  arrange(i.prev.dx.B) %>%
  filter(
    between(i.prev.dx.B, prevs["i.prev.dx.B"] - m, prevs["i.prev.dx.B"] + m),
    between(i.prev.dx.H, prevs["i.prev.dx.H"] - m, prevs["i.prev.dx.H"] + m),
    between(i.prev.dx.W, prevs["i.prev.dx.W"] - m, prevs["i.prev.dx.W"] + m)
  ) %>%
  print(n = 200) %>%
  pull(param_batch)

proposals[lst]

d2 <- df %>%
  group_by(param_batch) %>%
  summarise(across(
    c(i.prev.dx.B, i.prev.dx.H, i.prev.dx.W),
    list(
      q1 = ~ quantile(.x, prob = 0.25),
      q2 = ~ quantile(.x, prob = 0.5),
      q3 = ~ quantile(.x, prob = 0.75)
    )
  ))


df %>%
  group_by(param_batch) %>%
  summarise(across(
    c(ir100.gc, ir100.ct),
    list(
      # q1 = ~ quantile(.x, prob = 0.25),
      q2 = ~ quantile(.x, prob = 0.5)
      # q3 = ~ quantile(.x, prob = 0.75)
    )
  ))
