library(dplyr)
library(tidyr)
source("R/utils-labels.R")

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

make_outcomes <- function(baseline_file, scenarios_files,
                          scenarios_order = NULL) {
  # Calculate baseline elements
  df_baseline <- readRDS(baseline_file)

  if (! "found_indexes" %in% names(df_baseline)) {
    df_baseline$found_indexes <- 0
    df_baseline$elig_indexes <- 0
    df_baseline$found_partners <- 0
    df_baseline$elig_partners <- 0
    df_baseline$prepStartPart <- 0
  }

  df_base_cum <- df_baseline %>%
    filter(time >= max(time) - 52 * 10) %>%
    group_by(batch, sim) %>%
    summarise(
      cum_incid = sum(incid, na.rm = TRUE),
      cum_indexes = sum(found_indexes, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(
      cum_incid = median(cum_incid),
      cum_indexes = median(cum_indexes, na.rm = TRUE)
    )

    base_cum_incid <- df_base_cum$cum_incid
    base_cum_indexes <- df_base_cum$cum_indexes

    # Scenarios
    df_ls <- vector(mode = "list", length(scenarios_files))
    df_cur <- 0
    for (fle in scenarios_files) {
      df_sc <- readRDS(fle)

      if (! "found_indexes" %in% names(df_sc)) {
        df_sc$found_indexes <- 0
        df_sc$elig_indexes <- 0
        df_sc$found_partners <- 0
        df_sc$elig_partners <- 0
        df_sc$prepStartPart <- 0
      }

      df_cur <- df_cur + 1

      # outcome cumulated over intervention (10y)
      df_cum <- df_sc %>%
        filter(time >= max(time) - 52 * 10) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          cum_incid = sum(incid, na.rm = TRUE),
          cum_indexes = sum(found_indexes, na.rm = TRUE)
        ) %>%
        mutate(
          nia =  (base_cum_incid - cum_incid),
          pia = nia / base_cum_incid,
          # nnt = (cum_indexes - base_cum_indexes) / nia
          nnt = cum_indexes / nia
        ) %>%
        select(- cum_indexes) %>%
        ungroup()

      # Outcome at the end (mean over last year)
      df_at <- df_sc %>%
        filter(time >= max(time) - 52) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          ir100 = mean(incid / s___ALL * 5200, na.rm = TRUE),
          prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE),
          hiv_prev = mean(i___ALL / (i___ALL + s___ALL), na.rm = TRUE),
          hiv_diag = mean(i_dx___ALL / i___ALL, na.rm = TRUE),
          hiv_tx   = mean(i_tx___ALL / i_dx___ALL, na.rm = TRUE),
          hiv_supp = mean(i_sup___ALL / i_dx___ALL, na.rm = TRUE)
        ) %>%
        ungroup()

      # Part Serv process outcome
      df_part <- df_sc %>%
        filter(time >= max(time) - 10 * 52) %>%
        mutate(part_screened___ALL = part_spos___ALL + part_sneg___ALL) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          prep_start    = sum(prep_start___ALL, na.rm = TRUE),
          # part_ident    = sum(part_ident___ALL, na.rm = TRUE),
          part_screened = sum(part_screened___ALL, na.rm = TRUE),
          part_sneg     = sum(part_sneg___ALL, na.rm = TRUE),
          part_spos     = sum(part_spos___ALL, na.rm = TRUE),
          part_prep     = sum(prepStartPart, na.rm = TRUE),
          part_txinit   = sum(part_txinit___ALL, na.rm = TRUE),
          part_txreinit = sum(part_txreinit___ALL, na.rm = TRUE),
          elig_indexes  = sum(elig_indexes, na.rm = TRUE),
          found_indexes = sum(found_indexes, na.rm = TRUE),
          elig_partners = sum(elig_partners, na.rm = TRUE),
          found_partners = sum(found_partners, na.rm = TRUE),
          ident_dist0   = mean(ident_dist0___ALL, na.rm = TRUE),
          ident_dist1   = mean(ident_dist1___ALL, na.rm = TRUE),
          ident_dist2   = mean(ident_dist2___ALL, na.rm = TRUE),
          ident_dist3p  = mean(ident_dist3p___ALL, na.rm = TRUE),

          prep_time_on = mean(prep_time_on___ALL, na.rm = TRUE),
          prep_episodes = mean(prep_episodes___ALL, na.rm = TRUE)
        ) %>%
        mutate(
          prop_found_indexes = found_indexes / elig_indexes,
          prop_found_partners = found_partners / elig_partners,
          partners_found_per_indexes = found_partners / found_indexes,
          ident_sum    = ident_dist0 + ident_dist1 + ident_dist2 + ident_dist3p,
          ident_dist0  = ident_dist0 / ident_sum,
          ident_dist1  = ident_dist1 / ident_sum,
          ident_dist2  = ident_dist2 / ident_sum,
          ident_dist3p = ident_dist3p / ident_sum
        ) %>%
        select(-ident_sum) %>%
        ungroup()

      # binding of the dfs and formatting
      df_res <- df_cum %>%
        left_join(df_at, by = c("scenario", "batch", "sim")) %>%
        left_join(df_part, by = c("scenario", "batch", "sim"))

      df_ls[[df_cur]] <- df_res
    }

    df_out <- bind_rows(df_ls)

    if (!is.null(scenarios_order))
      df_out <- left_join(df_out, data.frame(scenario = scenarios_order))

    df_out
}

make_table <- function(df_res, ql = 0.025, qm = 0.5, qh = 0.975) {
   # this lines print the df with the variable in the right order
  df_res <- df_res %>%
    sum_quants(ql, qm, qh) %>%
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
      # scenario = scenarios_labels[scenario],
      name = var_labels[name]
    ) %>%
    pivot_wider(names_from = name, values_from = clean_val) %>%
    arrange(scenario)

  df_res <- df_res[, c("scenario", var_labels)] %>%
    select(-starts_with("__ignore__"))

  left_join(data.frame(scenario = scenarios), df_res, by = "scenario")
}


make_cum_dfs <- function(baseline_file, scenarios_files) {
  df_baseline <- readRDS(baseline_file)

  if (! "found_indexes" %in% names(df_baseline)) {
    df_baseline$found_indexes <- 0
    df_baseline$elig_indexes <- 0
    df_baseline$found_partners <- 0
    df_baseline$elig_partners <- 0
  }

  df_base_cum <- df_baseline %>%
    filter(time >= max(time) - 52 * 10) %>%
    group_by(batch, sim) %>%
    summarise(
      cum_incid = sum(incid, na.rm = TRUE),
      cum_indexes = sum(found_indexes, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(
      cum_incid = median(cum_incid),
      cum_indexes = median(cum_indexes, na.rm = TRUE)
    )

    base_cum_incid <- df_base_cum$cum_incid
    base_cum_indexes <- df_base_cum$cum_indexes

    # Scenarios
    df_ls <- vector(mode = "list", length(scenarios_files))
    df_cur <- 0
    for (fle in scenarios_files) {
      df_sc <- readRDS(fle)
      if (! "found_indexes" %in% names(df_sc)) {
        df_sc$found_indexes <- 0
        df_sc$elig_indexes <- 0
        df_sc$found_partners <- 0
        df_sc$elig_partners <- 0
      }

      df_cur <- df_cur + 1

      # outcome cumulated over intervention (10y)
      df_cum <- df_sc %>%
        filter(time >= max(time) - 52 * 10) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          cum_incid = sum(incid, na.rm = TRUE),
          cum_indexes = sum(found_indexes, na.rm = TRUE)
        ) %>%
        mutate(
          nia =  (base_cum_incid - cum_incid),
          pia = nia / base_cum_incid,
          # nnt = (cum_indexes - base_cum_indexes) / nia
          nnt = cum_indexes / nia
        ) %>%
        select(- cum_indexes) %>%
        ungroup()

      df_ls[[df_cur]] <- df_cum
    }

    bind_rows(df_ls)
}
