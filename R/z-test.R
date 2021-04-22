library(dplyr)
library(tidyr)

df <- readRDS("out/scdf.rds")
# c("no_ident", "ident_max")

scenarios_labels <- c(
  "no_ident_no_prep"          = "Neither Partner Services or PrEP",
  "no_ident"                  = "No Partner Services",
  "base_atlanta_complete"     = "Atlanta Complete",
  "base_atlanta_missing"      = "Atlanta Missing",
  "base_atlanta_complete_alt" = "Atlanta Complete Alt",
  "ident_max"                 = "All Max",
  "ident_max_test"            = "Max ID + Test",
  "ident_max_prep"            = "Max ID + Test + PrEP",
  "ident_max_tx"              = "Max ID + Test + Tx",
  "test_100"                  = "ATL ID & Max Test",
  "prep_100"                  = "ATL ID & Max PrEP",
  "test_prep_100"             = "ATL ID & Max Test + PrEP",
  "tx_100"                    = "ATL ID & Max Tx",
  "ident_x2"                  = "ALT ID x2",
  "ident_x2_test_100"         = "ATL ID x2 & Max Test",
  "ident_x2_prep_100"         = "ATL ID x2 & Max PrEP",
  "ident_x2_test_prep_100"    = "ATL ID x2 & Max Test + PrEP",
  "ident_x2_tx_100"           = "ATL ID x2 & Max Tx"
)

var_labels <- c(
 "nia"           = "nia",
 "pia"           = "pia",
 "ir100"         = "ir100",
 "prep_cov"      = "prep_cov",
 "hiv_diag"      = "hiv_diag",
 "hiv_tx"        = "hiv_tx",
 "hiv_supp"      = "hiv_supp",
 "part_ident"    = "part_ident",
 "part_screened" = "part_screened",
 "part_sneg"     = "part_sneg",
 "part_spos"     = "part_spos",
 "part_prep"     = "part_prep",
 "part_txinit"   = "part_txinit",
 "part_txreinit" = "part_txreinit"
)

fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)
fmts[["ir100"]] <- scales::label_number(0.01)
fmts[["pia"]] <- scales::label_percent(1)
fmts[["prep_cov"]] <- scales::label_percent(1)
fmts[["hiv_diag"]] <- scales::label_percent(1)
fmts[["hiv_tx"]] <- scales::label_percent(1)
fmts[["hiv_supp"]] <- scales::label_percent(1)

sum_quants <- function(df) {
  df %>%
    select(-sim) %>%
    group_by(scenario) %>%
    summarise(across(
      everything(),
      list(
        l = ~ quantile(.x, 0.025, na.rm = TRUE),
        m = ~ quantile(.x, 0.5, na.rm = TRUE),
        h = ~ quantile(.x, 0.975, na.rm = TRUE)
      ),
      .names = "{.col}_/_{.fn}"
    ))
}

cur_scenarios <- c( # first 1 is the reference for NIA PIA
  "no_ident",
  "ident_max",
  "ident_max_test",
  "ident_max_prep",
  "ident_max_tx"
)

cur_labels <- scenarios_labels[cur_scenarios]
cur_labels <- paste0(seq_along(cur_labels), "-", cur_labels)
names(cur_labels) <- cur_scenarios


df <- filter(df, scenario %in% cur_scenarios)

# outcome cumulated over intervention (15y)
df_cum <- df %>%
  filter(time >= max(time) - 52 * 15) %>%
  group_by(scenario, sim) %>%
  summarise(cum_incid = sum(incid, na.rm = TRUE))

base_cum_incid <- df_cum %>%
  filter(scenario == cur_scenarios[1]) %>%
  summarise(cum_incid = median(cum_incid)) %>%
  pull(cum_incid)

df_cum <- df_cum %>%
  group_by(scenario,sim) %>%
  summarise(
    nia =  (base_cum_incid - cum_incid),
    pia = nia / base_cum_incid
  )

df_cum %>% sum_quants()

# Outcome at the end (mean over last year)
df_at <- df %>%
  filter(time >= max(time) - 52) %>%
  group_by(scenario, sim) %>%
  summarise(
    ir100 = mean(ir100, na.rm = TRUE),
    prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE),
    hiv_diag = mean(i_dx___ALL / i___ALL, na.rm = TRUE),
    hiv_tx   = mean(i_tx___ALL / i_dx___ALL, na.rm = TRUE),
    hiv_supp = mean(i_sup___ALL / i_dx___ALL, na.rm = TRUE)
  )

df_at %>% sum_quants()

# Part Serv process outcome
#
# cum or mean?
# 1 year or full interv?

df_part <- df %>%
  filter(time >= max(time) - 52) %>%
  mutate(part_screened___ALL = part_spos___ALL + part_sneg___ALL) %>%
  group_by(scenario, sim) %>%
  summarise(
    part_ident    = sum(part_ident___ALL, na.rm = TRUE),
    part_screened = sum(part_screened___ALL, na.rm = TRUE),
    part_sneg     = sum(part_sneg___ALL, na.rm = TRUE),
    part_spos     = sum(part_spos___ALL, na.rm = TRUE),
    part_prep     = sum(part_prep___ALL, na.rm = TRUE),
    part_txinit   = sum(part_txinit___ALL, na.rm = TRUE),
    part_txreinit = sum(part_txreinit___ALL, na.rm = TRUE)
  )

df_res <- df_cum %>%
  left_join(df_at, by = c("scenario", "sim")) %>%
  left_join(df_part, by = c("scenario", "sim")) %>%
  sum_quants() %>%
  pivot_longer(-scenario) %>%
  separate(name, into = c("name", "quantile"), sep = "_/_") %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  mutate(
    clean_val = purrr::pmap_chr(
      list(name, l, m, h),
      ~ paste0(fmts[[..1]](..3), " (", fmts[[..1]](..2), ", ", fmts[[..1]](..4), ")")
    )
  ) %>%
  select(-c(l, m, h))
