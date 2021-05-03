library(tidyverse)
library(RcppRoll)
theme_set(theme_classic())

source("R/utils-labels.R")

# Read the data extracted in R/21-<...>.R
df <- readRDS("out/scdf.rds")

cur_scenarios <- c(
  "no_ident_no_prep",
  "no_ident",
  "ident_max",
  # "base_atlanta_missing",
  "base_atlanta_complete",
  "ident_max_test",
  "ident_max_prep",
  "ident_max_tx"
)

cur_labels <- make_ordered_labels(cur_scenarios, scenarios_labels)

df_b <- df

df <- df_b %>%
  filter( scenario %in% cur_scenarios) %>%
  mutate(scenario = cur_labels[scenario])

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
  paste0("out/plots/standardized_incid.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)
