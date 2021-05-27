library(tidyverse)
theme_set(theme_light())

jobs <- readRDS("out/calib_jobs.rds")
df_b <- map_dfr(jobs, ~ as_tibble(.x$data))

param_proposals <- jobs[[1]]$infos$param_proposals

df <- df_b

df %>%
  filter(time > max(time) - 104) %>%
  group_by(param_batch) %>%
  summarise(
    coverage = median(s_prep___ALL / s_prep_elig___ALL),
    time_on = median(prep_time_on___ALL),
    retention_1y = median(prep_1y___ALL)
  ) |> print()

param_proposals[1:2]

df %>%
  filter(param_batch <= 2) %>%
ggplot(aes(x = time/52, y = s_prep___ALL / s_prep_elig___ALL, col = as.character(param_batch))) +
  geom_smooth() +
  geom_hline(yintercept = 0.15)


df %>%
  filter(param_batch <= 2) %>%
ggplot( aes(x = time/52, y = i___ALL / num, col = as.character(param_batch))) +
  geom_smooth()

