library(data.table)

# One or many job_names
job_names <- "CPN_scenarios_all"
job_last_n <- 2 # if not NULL, get last N jobs. Otherwise, use job_names

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
    dff[, `:=`(batch = btch, scenario = names(infos$param_proposals)[btch])]
    df_ls[[btch]] <- dff
  }
  jobs[[job]]$data <- rbindlist(df_ls, fill = TRUE)
}

library(tidyverse)
theme_set(theme_classic())

df <- map_dfr(jobs, ~ as_tibble(.x$data))
df_b <- df

plot_time_quants <- function(df, y_label, interval = c(0.25, 0.75)) {
  df %>%
    group_by(scenario, time) %>%
    summarise(
      q1 = quantile(y, interval[1], na.rm = TRUE),
      q2 = quantile(y, 0.5, na.rm = TRUE),
      q3 = quantile(y, interval[2], na.rm = TRUE)
      ) %>%
    ggplot(aes(x = time / 52, y = q2, ymin = q1, ymax = q3,
        col = scenario, fill = scenario)) +
    geom_vline(xintercept = 65, col = "gray") +
    geom_vline(xintercept = 70, col = "gray") +
    geom_ribbon(alpha = 0.3, size = 0) +
    geom_line() +
    xlab("Time (years)") +
    ylab(y_label)
}

plot_time_smooth <- function(df, y_label) {
  df %>%
    ggplot(aes(x = time / 52, y = y, col = scenario, fill = scenario)) +
    geom_vline(xintercept = 65, col = "gray") +
    geom_vline(xintercept = 70, col = "gray") +
    geom_smooth() +
    xlab("Time (years)") +
    ylab(y_label)
}

scenarios_labels <- c(
    "no_ident"              = "0 - No Partner Services",
    "no_ident_no_prep"      = "1 - Neither Partner Services or PrEP",
    "base_atlanta_missing"  = "3 - Partner Services (Atlanta Missing)",
    "base_atlanta_complete" = "4 - Partner Services (Atlanta Complete)",
    "ident_prob1"           = "5 - Partner Services (all probs = 100%)"
  )


df <- df_b %>%
  filter(scenario %in% names(scenarios_labels)) %>%
  mutate(scenario = scenarios_labels[scenario])

# prep
df %>%
  filter(scenario == scenarios_labels["no_ident"]) %>%
  mutate(y = s_prep___all / s_prep_elig___all) %>%
  plot_time_quants("prep coverage (among eligibles)", c(0.025, 0.975)) +
  geom_hline(yintercept = 0.15, linetype = 2, alpha = .8) +
  expand_limits(y = c(0, 0.25))

ggsave(
  paste0("out/plots/cdc_presentation_prep.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

# prev

df %>%
  mutate(y = i___all / (s___all + i___all)) %>%
  plot_time_smooth("hiv prevalence") +
  expand_limits(y = c(0.2, 0.3))

ggsave(
  paste0("out/plots/cdc_presentation_prev.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

df %>%
  mutate(y = i_dx___all /  i___all) %>%
  plot_time_smooth("proportion of infected who are diagnosed") +
  expand_limits(y = c(0.8, 0.9))

ggsave(
  paste0("out/plots/cdc_presentation_idx.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

df %>%
  mutate(y = i_tx___all /  i___all) %>%
  plot_time_smooth("proportion of infected who are treated") +
  expand_limits(y = c(0.45, 0.5))

ggsave(
  paste0("out/plots/cdc_presentation_itx.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

df %>%
  mutate(y = i_tx___all /  i_dx___all) %>%
  plot_time_smooth("proportion of diagnosed who are treated") +
  expand_limits(y = c(0.55, 0.6))

ggsave(
  paste0("out/plots/cdc_presentation_itxdx.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

df %>%
  mutate(y = i_sup___all /  i_dx___all) %>%
  plot_time_smooth("proportion of diagnosed who are suppressed") +
  expand_limits(y = c(0.55, 0.6))

ggsave(
  paste0("out/plots/cdc_presentation_isupdx.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

df %>%
  mutate(y = ir100) %>%
  plot_time_smooth("standardized incidence") +
  expand_limits(y = c(1.2, 1.7))

ggsave(
  paste0("out/plots/cdc_presentation_incid.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

rmarkdown::render("rmd/cdc_presentation.rmd", output_dir = "out/renders/",
                  knit_root_dir = here::here())
