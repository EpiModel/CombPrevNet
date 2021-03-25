library(tidyverse)

files_folder <- "out/CPN_restart_select/out" # where are the calibration files
reprocess <- TRUE # set to TRUE to redo the file processing

# prepare targets
source("R/utils-targets.R")
target_names <- names(targets)

for (l in c("H", "B", "W")) {
  names(targets) <- str_replace(
    names(targets),
    paste0(".", l), paste0("___", l)
  )
}

tgts <- str_split(names(targets), "___")
tgts <- transpose(tgts)
tgts <- map(tgts, as.character)

df_targets <- tibble(
  target = tgts[[1]],
  pop = tgts[[2]],
  value = targets
)

df_targets <- df_targets %>%
  mutate(pop = if_else(pop == "NULL", "a", pop))

races <- c(
  "B" = "Black",
  "H" = "Hispanic",
  "W" = "White/Other",
  "a" = "All"
)

df_targets$pop <- races[df_targets$pop]

# Process files ----------------------------------------------------------------
if (reprocess) {
  get_targets <- function(df_part, tgt, target_names) {
    df_tmp <- df_part %>%
      select(c(time, all_of(target_names))) %>%
      filter(time %% 4 == T) %>%
      select(c(time, starts_with(tgt)))

    if (ncol(df_tmp) > 3) {
      df_tmp <- df_tmp %>%
        pivot_longer(-time) %>%
        separate(name, into = c("name", "pop"), sep = -1) %>%
        filter(pop != "x")
    } else {
      df_tmp <- mutate(df_tmp, pop = "a")
      names(df_tmp)[names(df_tmp) == tgt] <- "value"
    }

    df_tmp <- df_tmp %>%
      mutate(name = tgt) %>%
      select(time, name, pop, value)

    df_tmp
  }

  filenames <- fs::dir_ls(files_folder)

  n <- 1
  tgt_names <- unique(df_targets$target)
  names(tgt_names) <- tgt_names
  dir_part <- "out/part_dfs"

  if (!fs::dir_exists(dir_part))
    fs::dir_create(dir_part)

  for (fle in filenames) {
    df_part <- as_tibble(readRDS(fle))

    l_dfs <- lapply(tgt_names, get_targets,
      df_part = df_part, target_names = target_names)

    for (tgt in tgt_names) {
      saveRDS(l_dfs[[tgt]], fs::path(dir_part, paste0(tgt, "_", n, ".rds")),
        compress = FALSE)
    }

    n <- n + 1
  }

  # Make df files ----------------------------------------------------------------
  for (tgt in tgt_names) {
    filenames <- fs::dir_ls(dir_part, regexp = tgt)
    tgt_file <- paste0("out/df_calplot_", tgt, ".rds")

    if (!file.exists(tgt_file)) {
      df_calib <- bind_rows(lapply(filenames, readRDS))
      df_calib <- df_calib %>%
        group_by(time, pop) %>%
        summarise(
          q1 = quantile(value, prob = 0.25, na.rm = TRUE),
          q2 = quantile(value, prob = 0.5, na.rm = TRUE),
          q3 = quantile(value, prob = 0.75, na.rm = TRUE)
        )
        gc()
        df_calib$pop <- races[df_calib$pop]
        saveRDS(df_calib, tgt_file)
    }
    print(paste0("Finised: ", tgt))
  }

}

# Plots ------------------------------------------------------------------------
if (!fs::dir_exists("out/plots"))
  fs::dir_create("out/plots")

# Prev.dx
tgt <- "i.prev.dx"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = pop, label = value, x = 62),
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

# Diags
tgt <- "cc.dx"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.04, color = pop, label = value, x = 61),
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

# Linked1m
tgt <- "cc.linked1m"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = pop, label = value, x = 62),
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
tgt <- "cc.vsupp"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 0.01, color = pop, label = value, x = 62),
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

# ir100.gc
tgt <- "ir100.gc"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 2, color = pop, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0, 40), breaks = seq(0, 40, 5)) +
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
  ylab("GC Incidence Per 100 PYAR") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)

# ir100.ct
tgt <- "ir100.ct"
df_tgts <- df_targets %>%
  filter(target == tgt)

df_calib <- readRDS(paste0("out/df_calplot_", tgt, ".rds"))

ggplot(
  df_calib,
  aes(x = time / 52, y = q2, ymin = q1, ymax = q3, color = pop, fill = pop)
) +
  geom_hline(
    data = df_tgts, aes(yintercept = value, color = pop),
    linetype = 2, alpha = .8
  ) +
  geom_ribbon(alpha = 0.5, size = 0) +
  geom_line(size = 0.5) +
  geom_text(
    data = df_tgts,
    aes(y = value + 2, color = pop, label = value, x = 62),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0, 40), breaks = seq(0, 40, 5)) +
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
  ylab("CT Incidence Per 100 PYAR") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  paste0("out/plots/additional_calib_", tgt, ".jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)
