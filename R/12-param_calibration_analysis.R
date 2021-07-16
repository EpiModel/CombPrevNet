library(tidyverse)
theme_set(theme_light())

jobs <- readRDS("out/calib_jobs.rds")
df_b <- map_dfr(jobs, ~ as_tibble(.x$data))

param_proposals <- jobs[[1]]$infos$param_proposals

# df <- mutate(df_b, param_batch = 1)
df <- df_b

df <- df_b %>%
  group_by(param_batch) %>%
  summarise(
    ir100.gc = median(ir100.gc, na.rm = TRUE),
    ir100.ct = median(ir100.ct, na.rm = TRUE),
    i.prev.dx___B = median(i_dx___B / n___B, na.rm = TRUE),
    cc.dx___B = median(i_dx___B / i___B, na.rm = TRUE),
    cc.linked1m___B = median(linked1m___B / i_dx___B, na.rm = TRUE),
    cc.vsupp___B = median(i_sup___B / i_dx___B, na.rm = TRUE),
    i.prev.dx___H = median(i_dx___H / n___H, na.rm = TRUE),
    cc.dx___H = median(i_dx___H / i___H, na.rm = TRUE),
    cc.linked1m___H = median(linked1m___H / i_dx___H, na.rm = TRUE),
    cc.vsupp___H = median(i_sup___H / i_dx___H, na.rm = TRUE),
    i.prev.dx___W = median(i_dx___W / n___W, na.rm = TRUE),
    cc.dx___W = median(i_dx___W / i___W, na.rm = TRUE),
    cc.linked1m___W = median(linked1m___W / i_dx___W, na.rm = TRUE),
    cc.vsupp___W = median(i_sup___W / i_dx___W, na.rm = TRUE)
  ) %>%
  mutate(
    i.prev.dx___B = i.prev.dx___B - 0.33,
    i.prev.dx___H = i.prev.dx___H - 0.127,
    i.prev.dx___W = i.prev.dx___W - 0.084,
    cc.dx___B = cc.dx___B - 0.804,
    cc.dx___H = cc.dx___H - 0.799,
    cc.dx___W = cc.dx___W - 0.88,
    cc.linked1m___B = cc.linked1m___B - 0.62,
    cc.linked1m___H = cc.linked1m___H - 0.65,
    cc.linked1m___W = cc.linked1m___W - 0.76,
    cc.vsupp___B = cc.vsupp___B - 0.55,
    cc.vsupp___H = cc.vsupp___H - 0.60,
    cc.vsupp___W = cc.vsupp___W - 0.72
  )

thresh <- 0.0015
df %>%
  select(param_batch, starts_with("i.prev")) %>%
  filter(
    abs(i.prev.dx___B) < thresh,
    abs(i.prev.dx___H) < thresh,
    abs(i.prev.dx___W) < thresh
  )

param_proposals[c(24, 39, 47, 48)]


df %>%
  filter(time > max(time) - 52 * 10) %>%
  group_by(param_batch) %>%
  summarise(across(- c(time, )
  ) |> print(n = 200)

df %>%
  filter(time > max(time) - 52 * 10) %>%
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
  ggplot(aes(
    x = time/52, y = s_prep___ALL / s_prep_elig___ALL,
    col = as.character(param_batch)
  )) +
  geom_smooth()

df %>%
  filter(
    time > 52 * 10,
    time %% 10 == 0
  ) %>%
  ggplot(aes(
    x = time/52, y = ir100.gc,
    col = as.character(param_batch)
  )) +
  geom_smooth()


df %>%
  filter(
    time > max(time) - 52 * 6,
  ) %>%
  group_by(param_batch) %>%
  summarise(
    elic = mean(found_partners / found_indexes, na.rm = T),
    found = mean(found_partners / elig_partners, na.rm = T)
  ) %>%
  print(n = 200)

param_proposals[5]

df %>%
  filter(
    time > max(time) - 52 * 2,
  ) %>%
  group_by(param_batch) %>%
  summarise(psp = sum(prepStartPart, na.rm = T)) |>
  print(n = 200)
