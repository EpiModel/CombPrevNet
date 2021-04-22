library(dplyr)
library(tidyr)

# Uncomment one at a time
#
# res_name defines the name of the table
# cur_scenarios defines the scenarios to include
res_name <- ("No_ident_vs_Max")
cur_scenarios <- c( # first 1 is the reference for NIA PIA
  "no_ident",
  "ident_max_test",
  "ident_max_prep",
  "ident_max_tx",
  "ident_max"
)

# res_name <- ("ATL_vs_up_serv")
# cur_scenarios <- c( # first 1 is the reference for NIA PIA
#   "base_atlanta_complete",
#   "test_100",
#   "prep_100",
#   "test_prep_100",
#   "tx_100"
# )

# res_name <- ("ATL_vs_IDx2")
# cur_scenarios <- c( # first 1 is the reference for NIA PIA
#   "base_atlanta_complete",
#   "ident_x2",
#   "ident_x2_test_100",
#   "ident_x2_prep_100",
#   "ident_x2_test_prep_100",
#   "ident_x2_x_100"
# )

# res_name <- ("ATL_flavors")
# cur_scenarios <- c( # first 1 is the reference for NIA PIA
#   "base_atlanta_complete",
#   "base_atlanta_missing",
#   "base_atlanta_complete_alt"
# )


# Read the data extracted in R/21-<...>.R
df <- readRDS("out/scdf.rds")

# Conversion between scenario name and final label
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

# Conversion between variable name and final label
var_labels <- c(
 # Epi
 "ir100"         = "Incidence Rate",
 "nia"           = "NIA",
 "pia"           = "PIA",
 # Process
 "prep_cov"      = "PrEP Coverage",
 "hiv_diag"      = "HIV+ Diagnosed",
 "hiv_tx"        = "HIV+ Treated",
 "hiv_supp"      = "HIV+ Virally Suppressed",
 # Part Process
 "part_ident"    = "Number of Identified Partners",
 "part_screened" = "Number of Screened Partners",
 "part_sneg"     = "Number of Screened Partners (neg)",
 "part_spos"     = "Number of Screened Partners (pos)",
 "part_prep"     = "Number of Partners who Started PrEP",
 "part_txinit"   = "Number of Partners who Started ART",
 "part_txreinit" = "Number of Partners who Restarted ART"
)

# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)
fmts[["ir100"]] <- scales::label_number(0.01)
fmts[["pia"]] <- scales::label_percent(0.1)
fmts[["prep_cov"]] <- scales::label_percent(0.1)
fmts[["hiv_diag"]] <- scales::label_percent(0.1)
fmts[["hiv_tx"]] <- scales::label_percent(0.1)
fmts[["hiv_supp"]] <- scales::label_percent(0.1)

# Snippet to turn the vector of variable value into 3 quantiles
sum_quants <- function(df, ql = 0.025, qm = 0.5, qh = 0.975) {
  df %>%
    select(-sim) %>%
    group_by(scenario) %>%
    summarise(across(
      everything(),
      list(
        l = ~ quantile(.x, ql, na.rm = TRUE),
        m = ~ quantile(.x, qm, na.rm = TRUE),
        h = ~ quantile(.x, qh, na.rm = TRUE)
      ),
      .names = "{.col}_/_{.fn}"
    ))
}

# Add numbers to the labels to force the order
cur_labels <- scenarios_labels[cur_scenarios]
cur_labels <- paste0(seq_along(cur_labels), "-", cur_labels)
names(cur_labels) <- cur_scenarios

# Keep only the scenarios of interest
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

# Part Serv process outcome
#
# cummulative or mean?
# 1 year or full interv?
# for now: sum over 1 year
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

# binding of the dfs and formatting
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
  select(-c(l, m, h)) %>%
  mutate(
    scenario = cur_labels[scenario],
    name = var_labels[name]
  ) %>%
  pivot_wider(names_from = name, values_from = clean_val) %>%
  arrange(scenario)

# this lines print the df with the variable in the right order
df_res[, c("scenario", var_labels)]

# create the folder if it does not exist
if (!fs::dir_exists("out/tables")) fs::dir_create("out/tables")

# write the csv
readr::write_csv(
  df_res[, c("scenario", var_labels)],
  paste0("out/tables/", res_name, ".csv")
)
