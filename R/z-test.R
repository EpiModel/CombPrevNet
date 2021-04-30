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
 # "elig_indexes"  = "Number of Eligible Indexes",
 # "found_indexes" = "Number of Indexes Found",
 # "elicitaion"    = "Index Elicitation",
 "part_ident"    = "Number of Identified Partners",
 "part_screened" = "Number of Screened Partners",
 "part_sneg"     = "Number of Screened Partners (neg)",
 "part_spos"     = "Number of Screened Partners (pos)",
 "part_prep"     = "Number of Partners who Started PrEP",
 "part_txinit"   = "Number of Partners who Started ART",
 "part_txreinit" = "Number of Partners who Restarted ART",
 "ident_dist0"   = "Identified Distribution: 0",
 "ident_dist1"   = "Identified Distribution: 1",
 "ident_dist2"   = "Identified Distribution: 2",
 "ident_dist3p"  = "Identified Distribution: 3+"
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
fmts[["ident_dist0"]] <- scales::label_percent(1)
fmts[["ident_dist1"]] <- scales::label_percent(0.001)
fmts[["ident_dist2"]] <- scales::label_percent(0.001)
fmts[["ident_dist3p"]] <- scales::label_percent(0.001)
fmts[["elicitaion"]] <- scales::label_percent(0.1)

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
    part_txreinit = sum(part_txreinit___ALL, na.rm = TRUE),
    # elig_indexes  = sum(elig_indexes, na.rm = TRUE),
    # found_indexes = sum(found_indexes, na.rm = TRUE),
    ident_dist0   = mean(ident_dist0___ALL, na.rm = TRUE),
    ident_dist1   = mean(ident_dist1___ALL, na.rm = TRUE),
    ident_dist2   = mean(ident_dist2___ALL, na.rm = TRUE),
    ident_dist3p  = mean(ident_dist3p___ALL, na.rm = TRUE),
  ) %>%
  mutate(
    # elicitaion    = found_indexes / elig_indexes,
    ident_sum     = ident_dist0 + ident_dist1 + ident_dist2 + ident_dist3p,
    ident_dist0   = ident_dist0 / ident_sum,
    ident_dist1   = ident_dist1 / ident_sum,
    ident_dist2   = ident_dist2 / ident_sum,
    ident_dist3p  = ident_dist3p / ident_sum
  ) %>%
  select(-ident_sum)

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

# library(tidyverse)
# library(RcppRoll)

# theme_set(theme_classic())

# plot_time_smooth <- function(df, y_label) {
#   df %>%
#     ggplot(aes(x = time / 52, y = y, col = scenario, fill = scenario)) +
#     geom_vline(xintercept = 65, col = "gray") +
#     # geom_vline(xintercept = 70, col = "gray") +
#     geom_smooth() +
#     xlab("Time (years)") +
#     ylab(y_label)
# }

# plot_time_roll <- function(df, y_label, roll = 13, interval = c(0.25, 0.75)) {
#   df %>%
#     # group_by(scenario, sim) %>%
#     # arrange(time) %>%
#     # mutate(y = roll_mean(y, n = roll, align = "right", fill = NA)) %>%
#     group_by(scenario, time) %>%
#     # summarise(m = mean(y, na.rm = T), s = sd(y, na.rm = T)) %>%
#     # mutate(
#     #   q1 = m - s,
#     #   q2 = m,
#     #   q3 = m + s
#     # ) %>%
#     summarise(
#       q1 = quantile(y, interval[1], na.rm = TRUE),
#       q2 = mean(y, na.rm = T), #quantile(y, 0.5, na.rm = TRUE),
#       q3 = quantile(y, interval[2], na.rm = TRUE)
#     ) %>%
#     ggplot(aes(x = time / 52, y = q2, ymin = q1, ymax = q3,
#         col = scenario, fill = scenario)) +
#     geom_vline(xintercept = 65, col = "gray") +
#     # geom_vline(xintercept = 70, col = "gray") +
#     geom_ribbon(alpha = 0.3, size = 0) +
#     geom_line() +
#     xlab("Time (years)") +
#     ylab(y_label)
# }


# df %>%
#   mutate(y = ir100) %>%
#   plot_time_roll("standardized incidence", roll = 100, c(.49, .51))

# df %>%
#   mutate(y = ir100) %>%
#   plot_time_smooth("standardized incidence") +
#   expand_limits(y = c(1.2, 1.7))

# df %>%
#   mutate(y = ir100) %>%
#   ggplot(aes(x = time / 52, y = y, col = scenario)) +
#   geom_smooth(
#     method = "loess"
#     # method = "gam", formula = y ~ s(x, bs = "cs")
#   )

# roll <- 52
# df %>%
#   mutate(y = ir100) %>%
#     group_by(scenario, time) %>%
#     summarise(
#       y = mean(y)
#     ) %>%
#     group_by(scenario) %>%
#     mutate(
#       y = roll_mean(y, n = roll, align = "right", fill = NA)
#     ) %>%
#     ggplot(aes(x = time / 52, y = y, col = scenario)) +
#     geom_vline(xintercept = 65, col = "gray") +
#     geom_line()
