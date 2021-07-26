library(data.table)

reprocess_all <- FALSE

# One or many job_names
job_names <- "CPN_sc_t5_base"
# job_names <- "k_CPN_sc_fig5"
job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n))
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)


needed_trackers <- c(
  "n",
  "i", "i_dx", "i_tx", "i_sup", "linked1m",
  "s", "s_prep", "s_prep_elig",
  "prep_start", "prep_time_on", "prep_1y", "prep_episodes",
  "part_ident", "part_sneg", "part_spos",
  "part_prep", "part_txinit", "part_txreinit",
  "ident_dist0", "ident_dist1", "ident_dist2", "ident_dist3p"
)

needed_pops <- c("ALL", "B", "H", "W")

needed_trackers <- vapply(
  needed_pops,
  function(pop) paste0(needed_trackers, "___", pop),
  needed_trackers
)

needed_cols <- c(
  "sim", "time", "batch", "param_batch", "num",
  "incid",
  "prepStartPart",
  "found_indexes", "elig_indexes",
  "found_partners", "elig_partners",
  needed_trackers
)

for (job in job_names) {
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  for (fle in sim_files) {
    btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
    scenario_name <- names(infos$param_proposals)[btch]

    sim_dir <- fs::path("out/parts/scenarios", scenario_name)
    if (!fs::dir_exists(sim_dir)) fs::dir_create(sim_dir, recurse = TRUE)

    part_file <- fs::path(sim_dir, paste0(job, "-", btch, ".rds"))
    if (reprocess_all || !fs::file_exists(part_file)) {
      sim <- readRDS(fle)
      dff <- as.data.table(sim)

      dff[, `:=`(batch = btch, scenario = scenario_name)]

      keep_cols <- intersect(needed_cols, names(dff))
      dff <- dff[, ..keep_cols]


      saveRDS(dff, fs::path(sim_dir, paste0(job, "-", btch, ".rds")))
    }
  }
}

scenario_dir <- "out/parts/scenarios"
if (!fs::dir_exists(scenario_dir)) fs::dir_create(scenario_dir, recurse = TRUE)

scenarios <- fs::dir_ls(scenario_dir)
for (sc in scenarios) {
  elts <- fs::path_split(sc)
  scenario_name <- elts[[1]][length(elts[[1]])]

  file_names <- fs::dir_ls(sc)
  df_ls <- lapply(file_names, readRDS)

  dfs <- rbindlist(df_ls, fill = TRUE)[, scenario := scenario_name]

  saveRDS(dfs, fs::path("out/scenarios", paste0(scenario_name, ".rds")))
}
