library(data.table)
library(dplyr)

# One or many job_names
job_names <- c("CPN_restart")
jobs <- list()

# Read targets
source("R/utils-targets.R")

needed_trackers <- c(
  "n",
  "i", "i_dx", "i_sup", "linked1m",
  "s", "s_prep", "s_prep_elig",
  "prep_time_on", "prep_1y",
  "gc_s", "ct_s", "gc_i", "ct_i"
)

needed_pops <- c("ALL", "B", "H", "W")

needed_trackers <- vapply(
  needed_pops,
  function(pop) paste0(needed_trackers, "___", pop),
  needed_trackers
)

needed_cols <- c(
  "sim", "time", "batch", "param_batch", "num",
  "incid", # "incid.B", "incid.H", "incid.W",
  "incid.gc", # "incid.gc.B", "incid.gc.H", "incid.gc.W",
  "incid.ct", # "incid.ct.B", "incid.ct.H", "incid.ct.W",
  needed_trackers
)

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
    dff[, `:=`(batch = btch)]

    ## Do caclucation here
    dff <- dff[ time > max(time) - 52 ]

    keep_cols <- intersect(needed_cols, names(dff))
    df_ls[[btch]] <- dff[, ..keep_cols]
  }
  jobs[[job]]$data <- rbindlist(df_ls)
}

df_b <- jobs[[1]]$data

df <- df_b %>%
  group_by(batch, sim) %>%
  summarise(
    i.prev.dx = median(i_dx___ALL / n___ALL, na.rm = TRUE),
    cc.dx = median(i_dx___ALL / i___ALL, na.rm = TRUE),
    cc.linked1m = median(linked1m___ALL / i___ALL, na.rm = TRUE),
    cc.vsupp = median(i_sup___ALL / i_dx___ALL, na.rm = TRUE)
    # ir100.gc = incid.gc / s_gc___ALL * 5200,
    # ir100.ct = incid.ct / s_ct___ALL * 5200
  ) %>%
  mutate(
    i.prev.dx = i.prev.dx - 0.25,
    cc.dx = cc.dx - 0.82,
    cc.linked1m = cc.linked1m - 0.64,
    cc.vsupp = cc.vsupp - 0.65,
    total = i.prev.dx^2 + cc.dx^2 + cc.linked1m^2 + cc.vsupp^2
  ) %>%
  arrange(total)


# Best == batch44 sim19
sim <- readRDS(fs::path("out/remote_jobs/", job, "out/sim116.rds"))
orig <- EpiModel::get_sims(sim, 19)
saveRDS(orig, "out/est/restart.rds")


dt_norm <- df[
  ,
  Map(function(x, y) (x - y), .SD, targets),
  by = c("batch", "sim"), .SDcols = names(targets)
][,
  score := sum(.SD / sd(.SD))^2,
  by = c("batch", "sim"), .SDcols = names(targets)
][order(score)]

