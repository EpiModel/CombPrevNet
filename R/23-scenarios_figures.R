library(tidyverse)
library(RcppRoll)
library(metR)
theme_set(theme_classic())

source("R/utils-labels.R")
source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

if (!fs::dir_exists("out/plots/"))
  fs::dir_create("out/plots")

if (!fs::dir_exists("out/figures_data/"))
  fs::dir_create("out/figures_data/")

# Figure 1 ---------------------------------------------------------------------
scenarios <- names(c(sc_base, sc_t4))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df_ls <- vector(mode = "list", length(scenarios_files))
df_num <- 0
for (fle in scenarios_files) {
  df_num <- df_num + 1
  dff <- readRDS(fle)
  dff <- dff %>%
    select(scenario, batch, sim, time, incid, s___ALL) %>%
    mutate(ir100 = incid / s___ALL * 5200) %>%
    select(- c(incid, s___ALL))

  df_ls[[df_num]] <- dff
}

dff <- bind_rows(df_ls)

# Read the data extracted in R/21-<...>.R

df <- dff
saveRDS(df, "out/figures_data/figure1.rds")

# One year double smoothing
df %>%
  group_by(scenario, time) %>%
  mutate(ir100 = roll_mean(ir100, n = 52, align = "right", fill = NA)) %>%
  summarise(
    y = median(ir100, na.rm = TRUE),
    yl = quantile(ir100, prob = 0.25, na.rm = TRUE),
    yh = quantile(ir100, prob = 0.75, na.rm = TRUE)
  ) %>%
  mutate(across(
      c(yl, y, yh),
      ~ roll_mean(.x, n = 52, align = "right", fill = NA)
  )) %>%
  ggplot(aes(x = time / 52, y = y, ymin = yl, ymax = yh,
             col = scenario, fill = scenario)) +
  geom_vline(xintercept = 65 + 1/52, col = "gray") +
  geom_vline(xintercept = 70 + 1/52, col = "gray") +
  geom_line() +
  geom_ribbon(alpha = 0.1, linetype = "blank")

ggsave(
  paste0("out/plots/figure_1.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

# Figure 2 ---------------------------------------------------------------------
# fig2 index init * partner ident * pia
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig2)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("grp", "__1", "index", "__2", "partner"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(
    across(c(index, partner), as.numeric),
    grp = figure2_panel(grp)
  ) %>%
  group_by(grp, index, partner) %>%
  summarise(pia = median(pia))

saveRDS(df2, "out/figures_data/figure2.rds")

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = index, y = partner, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  # geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    # type = "viridis",
    # direction = -1,
    labels = scales::label_percent(),
    # limits = c(0, 1),
    name = "PIA"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  xlab("Index Initiation") +
  ylab("Partner Identification") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 1
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.1)) +
  ggtitle(outcome_label) +
  facet_grid(cols = vars(grp))

ggsave(
  paste0("out/plots/figure_2.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

# fig3 test * prep * pia -------------------------------------------------------
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig3)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("grp", "__1", "prep", "__2", "test"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(
    across(c(prep, test), as.numeric),
    grp = figure3_panel[grp]
  ) %>%
  group_by(grp, prep, test) %>%
  summarise(pia = median(pia))

saveRDS(df2, "out/figures_data/figure3.rds")

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = test, y = prep, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    type = "viridis",
    direction = -1,
    labels = scales::label_percent(),
    name = "PIA"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  xlab("Test") +
  ylab("PrEP") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 1
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.1)) +
  ggtitle(outcome_label) +
  facet_grid(cols = vars(grp))

ggsave(
  paste0("out/plots/figure_3.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 12,
  units = "in"
)

# fig4 prep * tx_reinit * pia --------------------------------------------------
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig4)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("grp", "__1", "tx", "__2", "test"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(
    across(c(tx, test), as.numeric),
    grp = figure4_panel[grp]
  ) %>%
  group_by(grp, tx, test) %>%
  summarise(pia = median(pia))

saveRDS(df2, "out/figures_data/figure4.rds")

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = test, y = tx, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    type = "viridis",
    direction = -1,
    labels = scales::label_percent(),
    name = "PIA"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = scales::label_percent()
  ) +
  xlab("HIV Screening") +
  ylab("Treatmet Linkage & Reengagement") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 1
    ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.1)) +
  ggtitle(outcome_label) +
  facet_grid(cols = vars(grp))

ggsave(
  paste0("out/plots/figure_4.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 12,
  units = "in"
)
