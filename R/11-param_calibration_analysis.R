library(data.table)

# One or many job_names
job_names <- c()
job_last_n <- 1 # if not NULL, get last N jobs. Otherwise, use job_names
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

df <- filter(df, time >= max(time) - 52)

df %>%
  group_by(param_batch) %>%
  summarise(across(all_of(names(targets)), median)) %>%
  select(ir100.gc, ir100.ct) %>%
  print(n = 200)


df %>%
  select(c(param_batch, starts_with("part_")))  %>%
  group_by(param_batch) %>%
  summarise(across(starts_with("part_"), median))

%>%
  pivot_longer(cols = -param_batch) %>%
  separate(name, c("name", "pop"), sep = "___") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(prep = s_prep / s_prep_elig)

  print(n = 200)

