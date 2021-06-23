library(tidyverse)

files_folder <- "out/remote_jobs/CPN_restart/out" # where are the calibration files
reprocess <- FALSE # set to TRUE to redo the file processing

process_1batch <- function(file_name, out_dir) {
  sim <- readRDS(file_name)
  dff <- as_tibble(sim)

  dff <- dff %>%
    group_by(sim, time) %>%
    mutate(
      ir100.gc = median(ir100.gc, na.rm = TRUE),
      ir100.ct = median(ir100.ct, na.rm = TRUE),
      i.prev.dx.B = median(i_dx___B / n___B, na.rm = TRUE),
      i.prev.dx.H = median(i_dx___H / n___H, na.rm = TRUE),
      i.prev.dx.W = median(i_dx___W / n___W, na.rm = TRUE),
      cc.dx.B = median(i_dx___B / i___B, na.rm = TRUE),
      cc.dx.H = median(i_dx___H / i___H, na.rm = TRUE),
      cc.dx.W = median(i_dx___W / i___W, na.rm = TRUE),
      cc.linked1m.B = median(linked1m___B / i_dx___B, na.rm = TRUE),
      cc.linked1m.H = median(linked1m___H / i_dx___H, na.rm = TRUE),
      cc.linked1m.W = median(linked1m___W / i_dx___W, na.rm = TRUE),
      cc.vsupp.B = median(i_sup___B / i_dx___B, na.rm = TRUE),
      cc.vsupp.H = median(i_sup___H / i_dx___H, na.rm = TRUE),
      cc.vsupp.W = median(i_sup___W / i_dx___W, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(
      time,
      i.prev.dx.B, i.prev.dx.H, i.prev.dx.W,
      cc.dx.B, cc.dx.H, cc.dx.W,
      cc.linked1m.B, cc.linked1m.H, cc.linked1m.W,
      cc.vsupp.B, cc.vsupp.H, cc.vsupp.W,
      ir100.gc, ir100.ct
    )

    saveRDS(dff, fs::path(out_dir, fs::path_file(file_name)), compress = FALSE)
}

dir_part <- "out/part_dfs"
if (reprocess) {
  future::plan(future::multicore, workers = 4)

  filenames <- fs::dir_ls(files_folder)

  n <- 1

  if (!fs::dir_exists(dir_part))
    fs::dir_create(dir_part)

  furrr::future_walk(filenames, process_1batch, out_dir = dir_part)
}

# Prepare for plots
source("R/utils-targets.R")
df_targets <- tibble(
  name = names(targets),
  value = targets
)

# prepare targets
target_groups <- list(
  i.prev.dx = c("i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W"),
  cc.dx = c("cc.dx.B", "cc.dx.H", "cc.dx.W"),
  cc.linked1m = c("cc.linked1m.B", "cc.linked1m.H", "cc.linked1m.W"),
  cc.vsupp = c("cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W"),
  sti = c("ir100.gc", "ir100.ct")
)

get_target_plot_df <- function(target_group) {
  filenames <- fs::dir_ls(dir_part)
  df_ls <- vector(mode = "list", length(filenames))
  num_df <- 0

  for (fle in filenames) {
    num_df <- num_df + 1
    df_ls[[num_df]]  <- readRDS(fle) %>%
      select(time, all_of(target_group))
  }
  bind_rows(df_ls)
}


# i.prev.dx
tgt <- "i.prev.dx"
dff <- get_target_plot_df(target_groups[[tgt]])
df_tgts <- df_targets %>%
  filter(name %in% target_groups[[tgt]])

df_calib <- dff %>%
  pivot_longer(-time) %>%
  group_by(time, name) %>%
  summarise(across(
    value,
    list(
      q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, prob = 0.5, na.rm = TRUE),
      q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
    )
  ))

ggplot(df_calib, aes(
    x = time / 52, y = value_q2, ymin = value_q1, ymax = value_q3,
    col = name, fill = name
  )) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = name),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = name, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0.08, 0.45), breaks = seq(0.05, 0.45, 0.05)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("HIV prevalence (diagnosed)") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)


# cc.dx
rm(dff)
rm(df_calib)
gc()
tgt <- "cc.dx"
dff <- get_target_plot_df(target_groups[[tgt]])
df_tgts <- df_targets %>%
  filter(name %in% target_groups[[tgt]])

df_calib <- dff %>%
  pivot_longer(-time) %>%
  group_by(time, name) %>%
  summarise(across(
    value,
    list(
      q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, prob = 0.5, na.rm = TRUE),
      q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
    )
  ))

ggplot(df_calib, aes(
    x = time / 52, y = value_q2, ymin = value_q1, ymax = value_q3,
    col = name, fill = name
  )) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = name),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.04, color = name, label = value, x = 61),
    inherit.aes = FALSE, size = 2
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0, 1), breaks = seq(0.0, 1, 0.1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Proportion of HIV Infected Diagnosed") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)


# cc.linked1m
rm(dff)
rm(df_calib)
gc()
tgt <- "cc.linked1m"
dff <- get_target_plot_df(target_groups[[tgt]])
df_tgts <- df_targets %>%
  filter(name %in% target_groups[[tgt]])

df_calib <- dff %>%
  pivot_longer(-time) %>%
  group_by(time, name) %>%
  summarise(across(
    value,
    list(
      q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, prob = 0.5, na.rm = TRUE),
      q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
    )
  ))

ggplot(df_calib, aes(
    x = time / 52, y = value_q2, ymin = value_q1, ymax = value_q3,
    col = name, fill = name
  )) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = name),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = name, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Proportion of Diagnosed Linked to Care Within a Month") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

# cc.vsupp
rm(dff)
rm(df_calib)
gc()
tgt <- "cc.vsupp"
dff <- get_target_plot_df(target_groups[[tgt]])
df_tgts <- df_targets %>%
  filter(name %in% target_groups[[tgt]])

df_calib <- dff %>%
  pivot_longer(-time) %>%
  group_by(time, name) %>%
  summarise(across(
    value,
    list(
      q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, prob = 0.5, na.rm = TRUE),
      q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
    )
  ))


ggplot(df_calib, aes(
    x = time / 52, y = value_q2, ymin = value_q1, ymax = value_q3,
    col = name, fill = name
  )) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = name),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = name, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("Proportion of Diagnosed Virally Suppressed") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

# STIs
rm(dff)
rm(df_calib)
gc()
tgt <- "sti"
dff <- get_target_plot_df(target_groups[[tgt]])
df_tgts <- df_targets %>%
  filter(name %in% target_groups[[tgt]])

df_calib <- dff %>%
  pivot_longer(-time) %>%
  group_by(time, name) %>%
  summarise(across(
    value,
    list(
      q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, prob = 0.5, na.rm = TRUE),
      q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
    )
  ))

ggplot(df_calib, aes(
    x = time / 52, y = value_q2, ymin = value_q1, ymax = value_q3,
    col = name, fill = name
  )) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = name),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = name, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  # scale_y_continuous(lim = c(0.4, 1), breaks = seq(0.4, 1, 0.1)) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(margin = margin(5, 0, 10, 0, "pt")),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10, "pt")),
    text = element_text(size = 10),
    aspect.ratio = 1/2
  ) +
  guides(
    col = guide_legend(title = "Race"),
    fill = guide_legend(title = "Race")
  ) +
  ylab("STI Standardized Incidence") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

