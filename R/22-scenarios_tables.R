library(dplyr)
library(tidyr)
source("R/utils-labels.R")

# Read the data extracted in R/21-<...>.R
df_orig <- readRDS("out/scdf.rds")

tables <- list(
  "No_ident_vs_Max" = c(
    "no_ident",
    "ident_max_test",
    "ident_max_prep",
    "ident_max_tx",
    "ident_max"
  ),
  "ATL_vs_up_serv" = c(
    "base_atlanta_complete",
    "test_100",
    "prep_100",
    "test_prep_100",
    "tx_100"
  ),
  "ATL_vs_IDx2"  = c(
    "base_atlanta_complete",
    "ident_x2",
    "ident_x2_test_100",
    "ident_x2_prep_100",
    "ident_x2_test_prep_100",
    "ident_x2_x_100"
  ),
  "ATL_flavors"  = c(
    "base_atlanta_complete",
    "base_atlanta_missing",
    "base_atlanta_complete_alt"
  )
)

# Snippet to turn the vector of variable value into 3 quantiles
sum_quants <- function(df, ql = 0.025, qm = 0.5, qh = 0.975) {
  df %>%
    ungroup() %>%
    select(-c(batch, sim)) %>%
    group_by(scenario) %>%
    summarise(across(
      everything(),
      list(
        l = ~ quantile(.x, ql, na.rm = TRUE),
        m = ~ quantile(.x, qm, na.rm = TRUE),
        h = ~ quantile(.x, qh, na.rm = TRUE)
      ),
      .names = "{.col}_/_{.fn}"
    )) %>%
  ungroup()
}

# For each table do:
for (i in seq_along(tables)) {
  res_name <- names(tables)[i]
  cur_scenarios <- tables[[i]]

  # Add numbers to the labels to force the order
  cur_labels <- make_ordered_labels(cur_scenarios, scenarios_labels)

  # Keep only the scenarios of interest
  df <- filter(df_orig, scenario %in% cur_scenarios)

  # outcome cumulated over intervention (15y)
  df_cum <- df %>%
    filter(time >= max(time) - 52 * 15) %>%
    group_by(scenario, batch, sim) %>%
    summarise(cum_incid = sum(incid, na.rm = TRUE)) %>%
    ungroup()

  base_cum_incid <- df_cum %>%
    filter(scenario == cur_scenarios[1]) %>%
    summarise(cum_incid = median(cum_incid)) %>%
    pull(cum_incid)

  df_cum <- df_cum %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      nia =  (base_cum_incid - cum_incid),
      pia = nia / base_cum_incid
      ) %>%
    ungroup()

  # Outcome at the end (mean over last year)
  df_at <- df %>%
    filter(time >= max(time) - 52) %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      ir100 = mean(ir100, na.rm = TRUE),
      prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE),
      hiv_diag = mean(i_dx___ALL / i___ALL, na.rm = TRUE),
      hiv_tx   = mean(i_tx___ALL / i_dx___ALL, na.rm = TRUE),
      hiv_supp = mean(i_sup___ALL / i_dx___ALL, na.rm = TRUE)
      ) %>%
    ungroup()


  # Part Serv process outcome
  #
  # cummulative or mean?
  # 1 year or full interv?
  # for now: sum over 1 year
  df_part <- df %>%
    filter(time >= max(time) - 52) %>%
    mutate(part_screened___ALL = part_spos___ALL + part_sneg___ALL) %>%
    group_by(scenario, batch, sim) %>%
    summarise(
      prep_start    = sum(prep_start___ALL, na.rm = TRUE),
      part_ident    = sum(part_ident___ALL, na.rm = TRUE),
      part_screened = sum(part_screened___ALL, na.rm = TRUE),
      part_sneg     = sum(part_sneg___ALL, na.rm = TRUE),
      part_spos     = sum(part_spos___ALL, na.rm = TRUE),
      part_prep     = sum(part_prep___ALL, na.rm = TRUE),
      part_txinit   = sum(part_txinit___ALL, na.rm = TRUE),
      part_txreinit = sum(part_txreinit___ALL, na.rm = TRUE),
      elig_indexes  = sum(elig_indexes, na.rm = TRUE),
      found_indexes = sum(found_indexes, na.rm = TRUE),
      ident_dist0   = mean(ident_dist0___ALL, na.rm = TRUE),
      ident_dist1   = mean(ident_dist1___ALL, na.rm = TRUE),
      ident_dist2   = mean(ident_dist2___ALL, na.rm = TRUE),
      ident_dist3p  = mean(ident_dist3p___ALL, na.rm = TRUE),

      prep_time_on = mean(prep_time_on___ALL, na.rm = TRUE),
      prep_episodes = mean(prep_episodes___ALL, na.rm = TRUE)
      ) %>%
    mutate(
      prop_found_indexes = found_indexes / elig_indexes,
      ident_sum          = ident_dist0 + ident_dist1 + ident_dist2 + ident_dist3p,
      ident_dist0        = ident_dist0 / ident_sum,
      ident_dist1        = ident_dist1 / ident_sum,
      ident_dist2        = ident_dist2 / ident_sum,
      ident_dist3p       = ident_dist3p / ident_sum
      ) %>%
    select(-ident_sum) %>%
    ungroup()

  # binding of the dfs and formatting
  df_res <- df_cum %>%
    left_join(df_at, by = c("scenario", "batch", "sim")) %>%
    left_join(df_part, by = c("scenario", "batch", "sim")) %>%
    sum_quants() %>%
    pivot_longer(-scenario) %>%
    separate(name, into = c("name", "quantile"), sep = "_/_") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    mutate(
      clean_val = purrr::pmap_chr(
        list(name, l, m, h),
        ~ paste0(
          fmts[[..1]](..3), " (", fmts[[..1]](..2),
          ", ", fmts[[..1]](..4), ")"
        )
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
}
