library(tidyverse)

sim <- readRDS(fs::path("out/remote_jobs/CPN_restart/out/sim460.rds"))
orig <- EpiModel::get_sims(sim, 18)
orig$epi <- orig$epi["num"] # keep only the "num" epi tracker

df_b <- as_tibble(sim)

df <- df_b %>%
  filter(time > max(time) - 52 * 10) %>%
  group_by(sim) %>%
  summarise(
    ir100.gc = median(ir100.gc, na.rm = TRUE),
    ir100.ct = median(ir100.ct, na.rm = TRUE),
    i.prev.dx___B = median(i_dx___B / n___B, na.rm = TRUE),
    cc.dx___B = median(i_dx___B / i___B, na.rm = TRUE),
    cc.linked1m___B = median(linked1m___B / i___B, na.rm = TRUE),
    cc.vsupp___B = median(i_sup___B / i_dx___B, na.rm = TRUE),
    i.prev.dx___H = median(i_dx___H / n___H, na.rm = TRUE),
    cc.dx___H = median(i_dx___H / i___H, na.rm = TRUE),
    cc.linked1m___H = median(linked1m___H / i___H, na.rm = TRUE),
    cc.vsupp___H = median(i_sup___H / i_dx___H, na.rm = TRUE),
    i.prev.dx___W = median(i_dx___W / n___W, na.rm = TRUE),
    cc.dx___W = median(i_dx___W / i___W, na.rm = TRUE),
    cc.linked1m___W = median(linked1m___W / i___W, na.rm = TRUE),
    cc.vsupp___W = median(i_sup___W / i_dx___W, na.rm = TRUE)
  )
