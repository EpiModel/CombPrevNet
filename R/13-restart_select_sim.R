library(data.table)

# One or many job_names
job_names <- c("CPN_restart_select")
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

  for (fle in sim_files) {
    btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
    sim <- readRDS(fle)
    dff <- as.data.table(sim)
    dff[, `:=`(batch = btch, param_batch = infos$unique_proposals[btch])]

    ## Do caclucation here
    dff <- dff[
      time > max(time) - 52,
      lapply(.SD, median, na.rm = T),
      .SDcols = names(targets), by = c("batch", "sim")
    ]

    df_ls[[btch]] <- dff
  }
  jobs[[job]]$data <- rbindlist(df_ls)
}

df <- jobs[[1]]$data

dt_norm <- df[
  ,
  Map(function(x, y) (x - y), .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
][,
  score := sum(.SD / sd(.SD))^2,
  by = c("batch", "sim"), .SDcols = names(targets)
][order(score)]

# Best == batch301 sim14
sim <- readRDS(fs::path("out/remote_jobs/", job, "out/sim301.rds"))
orig <- EpiModel::get_sims(sim, 14)
saveRDS(orig, "out/est/restart.rds")

## Check
#
# library(ggplot2)
# theme_set(theme_light())
#
# df <- as.data.table(orig)
# print(names(df), max = 200)
#
# plot_evol <- function(df, col, targets) {
#   tgt <- targets[col]
#   ggplot(df, aes(x = time, y = get(col))) +
#     ylab(col) +
#     ylim(tgt * 0.8, tgt * 1.2) +
#     geom_line() +
#     geom_hline(yintercept = tgt)
# }

# df_t <- df[time > max(time) - 52, ]

# for (n in names(targets)) {
#   print(n)
#   print(targets[n])
#   print(plot_evol(df_t, n, targets))
#   readline()
# }

