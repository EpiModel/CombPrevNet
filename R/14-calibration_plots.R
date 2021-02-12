library(tidyverse)
library(future.apply)
plan(multiprocess, workers = 4)

# prepare targets
source("R/utils-targets.R")

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
  pop = tgts[[2]]
)

df_targets <- df_targets %>%
  mutate(pop = if_else(pop == "NULL", "a", pop))

races <- c(
  "B" = "Black",
  "H" = "Hispanic",
  "W" = "White/Other",
  "a" = "All"
)

get_targets <- function(fle, tgt) {

  df_tmp <- as_tibble(readRDS(fle)) %>%
    filter(time %% 4 == T) %>%
    select(c(time, starts_with(tgt))) %>%
    pivot_longer(-time)

  if (ncol(df_tmp) > 3) {
    df_tmp <- separate(df_tmp, name, into = c("name", "pop"), sep = "-1")
  } else {
    df_tmp <- mutate(df_tmp, pop = "a")
  }

  df_tmp
}

filenames <- fs::dir_ls("out/CPN_restart_select/out")

for (tgt in unique(df_targets$target)) {
  tgt_file <- paste0("out/df_calplot_", tgt, ".rds")

  if (!file.exists(tgt_file)) {
    df_calib <- bind_rows(future_lapply(filenames, get_targets, tgt = tgt))
    df_calib <- df_calib %>%
      group_by(time, pop) %>%
      summarise(
        q1 = quantile(value, prob = 0.25, na.rm = TRUE),
        q2 = quantile(value, prob = 0.5, na.rm = TRUE),
        q3 = quantile(value, prob = 0.75, na.rm = TRUE)
      )
      gc()
      df_targets$pop <- races[df_targets$pop]
      saveRDS(df_targets, tgt_file)
  }
}

# Prev
df_calib <- readRDS(paste0("out/df_calplot_", "i.prev.dx", ".rds"))
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
    aes(y = value + 0.015, color = pop, label = value, x = 67),
    inherit.aes = FALSE, size = 3
  ) +
  theme_classic() +
  scale_y_continuous(lim = c(0.15, 0.55)) +
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
  ylab("HIV prevalence") +
  xlab("Time (years)") +
  ggtitle(" ")

ggsave(
  "out/plots/additional_calib_prev.jpeg",
  device = "jpeg", dpi = 600,
  height = 5, width = 7,
  units = "in"
)
rm(df_targets)
}

filenames <- fs::dir_ls("/media/BACKUP/home_backup/SFO_choose_restart_SFO/out")

# Prevalence ---
tgts <- targets[str_starts(names(targets), "prev")]
df_tgts <- tibble(name = names(tgts), value = tgts) %>%
  mutate(pop = races[str_sub(name, -1, -1)])

if (!file.exists("out/df_calplot_prev.rds")) {
  df_targets <- bind_rows(future_lapply(filenames, get_targets, tgts = tgts))
  df_targets <- df_targets %>%
    group_by(time, pop) %>%
    summarise(
      q1 = quantile(value, prob = 0.25, na.rm = TRUE),
      q2 = quantile(value, prob = 0.5, na.rm = TRUE),
      q3 = quantile(value, prob = 0.75, na.rm = TRUE)
    )
  gc()
  df_targets$pop <- races[df_targets$pop]
  saveRDS(df_targets, "out/df_calplot_prev.rds")
} else {
  df_targets <- readRDS("out/df_calplot_prev.rds")
}
