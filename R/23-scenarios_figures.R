library(tidyverse)
library(RcppRoll)
library(metR)
theme_set(theme_classic())

source("R/utils-labels.R")
source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

# Figure 1
scenarios <- names(sc_fig1)
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

# Figure 2
# fig2 index init * partner ident * pia
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig2)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("__s", "__i", "index", "__p", "partner"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(across(c(index, partner), as.numeric)) %>%
  group_by(index, partner) %>%
  summarise(pia = median(pia))

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = index, y = partner, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  # geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    # type = "viridis",
    # direction = -1,
    labels = scales::label_percent(),
    name = "PIA"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Index Initiation") +
  ylab("Partner Identification") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    aspect.ratio = 1
    ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.1)) +
  ggtitle(outcome_label)

ggsave(
  paste0("out/plots/figure_2.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

# fig3 prep * tx_reinit * pia
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig3)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("grp", "__p", "prep", "__t", "tx_reinit"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(across(c(prep, tx_reinit), as.numeric)) %>%
  group_by(grp, prep, tx_reinit) %>%
  summarise(pia = median(pia))

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = prep, y = tx_reinit, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    type = "viridis",
    direction = -1,
    labels = scales::label_percent(),
    name = "PIA"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
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
  paste0("out/plots/figure_3.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 12,
  units = "in"
)

# fig4 prep * tx_reinit * pia
base_file <- paste0("out/scenarios/", names(sc_base), ".rds")
scenarios <- names(sc_fig4)
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dff <- make_cum_dfs(base_file, scenarios_files)

df2 <- dff %>%
  separate(
    scenario,
    into = c("grp", "__p", "prep", "__t", "partner"),
    sep = "__"
  ) %>%
  select(- starts_with("__")) %>%
  mutate(across(c(prep, partner), as.numeric)) %>%
  group_by(grp, prep, partner) %>%
  summarise(pia = median(pia))

outcome_label <- "Percent of Infection Averted"
ggplot(df2, aes(x = prep, y = partner, z = pia)) +
  geom_contour_fill(na.fill = TRUE) +
  geom_contour(col = "white", alpha = 0.5, lwd = 0.5) +
  scale_fill_continuous(
    type = "viridis",
    direction = -1,
    labels = scales::label_percent(),
    name = "PIA"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
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
  paste0("out/plots/figure_4.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 12,
  units = "in"
)
