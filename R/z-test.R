library(tidyverse)
df <- readRDS("out/scenarios/base_atlanta_complete.rds")
df <- readRDS("out/scenarios/t2c_no_ident.rds")

df %>%
  filter(time > 52 * 70) %>%
  group_by(sim, batch) %>%
  summarise(
    # elig_indexes  = sum(elig_indexes, na.rm = TRUE),
    found_indexes = sum(found_indexes, na.rm = TRUE),
    # elig_partners = sum(elig_partners, na.rm = TRUE),
    # found_partners = sum(found_partners, na.rm = TRUE),
    found_partners2 = sum(part_ident___ALL, na.rm = TRUE),
    y = found_partners2 / found_indexes
  ) %>% pull(y) %>% summary()

df %>%
  filter(sim == 1, batch == 59) %>%
  pull(found_partners)

df %>%
  filter(sim == 1, batch == 59) %>%
  pull(part_ident___ALL)
