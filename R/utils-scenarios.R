
# Scenarios tables =============================================================
# Utilities
append_scenario_seq <- function(sc, sc_names, sc_params) {
  sc_params <- purrr::transpose(sc_params)
  for (i in seq_along(sc_params)) {
    scenar <- list(
      list(
        list(
          at = scenarios_update_time,
          param = sc_params[[i]]
        )
      )
    )

    names(scenar) <- sc_names[i]

    sc <- c(sc, scenar)
  }

  sc
}

append_scenario_cross <- function(sc, name_prefix, sc_fixed, sc_cross) {
  sc_cross <- purrr::transpose(sc_cross)
  for (i in seq_along(sc_cross)) {
    scenar <- list(
      list(
        list(
          at = scenarios_update_time,
          param = c(sc_fixed, sc_cross[[i]])
        )
      )
    )

    p_names <- names(sc_cross[[i]])
    p_vals <- vapply(sc_cross[[i]], function(x) x, 0)
    name <- paste0(
      name_prefix, "__",
      p_names[1], "__", p_vals[1], "__",
      p_names[2], "__", p_vals[2]
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

scenarios_update_time <- scenarios_update_time

increments <- c(1.1, 1.25, 1.50)
increments_names <- stringr::str_pad(increments, 4, "right", "0")

absolutes <- c(10, 25, 50, 75, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

windows <- c(26, 4)
windows_names <- stringr::str_pad(windows, 2, "left", "0")

degrees <- c(1, 2, 3, 5, 10)
degrees_names <- stringr::str_pad(degrees, 2, "left", "0")

# Base Scenario ----------------------------------------------------------------
sc_base <- list(base_atlanta_complete = list())

# Table 2 ----------------------------------------------------------------------
sc_t2 <- list()

# Index initiation
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_index_init_a", stringr::str_pad(c(7:10)*10, 3, "left", 0)),
  list(
    part.index.prob = list(0.7, 0.8, 0.9, 1)
  )
)

# Identification
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_part_ident_i", increments_names),
  list(
    part.ident.main.prob = as.list(param$part.ident.main.prob * increments),
    part.ident.casl.prob = as.list(param$part.ident.casl.prob * increments),
    part.ident.ooff.prob = as.list(param$part.ident.ooff.prob * increments)
  )
)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_part_ident_a", absolutes_names),
  list(
    part.ident.main.prob = as.list(absolutes),
    part.ident.casl.prob = as.list(absolutes),
    part.ident.ooff.prob = as.list(absolutes)
  )
)

# Testing
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_hiv_test_i", increments_names),
  list(
    part.hiv.test.rate = lapply(param$part.hiv.test.rate * increments, rep, 3)
  )
)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_hiv_test_a", absolutes_names),
  list(
    part.hiv.test.rate = lapply(absolutes, rep, 3)
  )
)

# PrEP linkage
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_prep_start_i", absolutes_names),
  list(
    part.prep.start.prob = lapply(absolutes, rep, 3)
  )
)

# Tx Init
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_init_i", increments_names),
  list(
    part.tx.init.prob = lapply(param$part.tx.init.prob * increments, rep, 3)
  )
)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_init_a", absolutes_names),
  list(
    part.tx.init.prob = lapply(absolutes, rep, 3)
  )
)

# Tx Reinit
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_reinit_a", absolutes_names),
  list(
    part.tx.reinit.prob = lapply(absolutes, rep, 3)
  )
)

# Table 3 ----------------------------------------------------------------------
# windows
sc_t3 <- list()
sc_t3 <- append_scenario_seq(
  sc_t3,
  paste0("t3_main_win_a", windows_names),
  list(
    part.ident.main.window = as.list(windows)
  )
)

sc_t3 <- append_scenario_seq(
  sc_t3,
  paste0("t3_casl_win_a", windows_names),
  list(
    part.ident.casl.window = as.list(windows)
  )
)

sc_t3 <- append_scenario_seq(
  sc_t3,
  paste0("t3_ooff_win_a", windows_names),
  list(
    part.ident.ooff.window = as.list(windows)
  )
)

# degrees
sc_t3 <- append_scenario_seq(
  sc_t3,
  paste0("t3_index_degree_a", degrees_names),
  list(
    part.index.degree = as.list(degrees)
  )
)

# Figure 1 ---------------------------------------------------------------------
sc_fig1 <- list(
  no_ident_no_prep = list(
    list(
      at = param$riskh.start - 1,
      param = list(
        part.ident.start = Inf,
        prep.start = Inf,
        riskh.start = Inf # not mandatory but faster
      )
    )
  ),
  no_ident = list(
    list(
      at = param$riskh.start - 1,
      param = list(
        part.ident.start = Inf
      )
    )
  ),
  ident_max_all = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum possible effect (unachievable in practice)
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(1, 3),
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  ),
  ident_max_test = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test (prep effect via LNT)
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(0, 3),
        part.tx.init.prob    = rep(0, 3),
        part.tx.reinit.prob  = rep(0, 3)
      )
    )
  ),
  ident_max_prep = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test + prep
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(1, 3),
        part.tx.init.prob    = rep(0, 3),
        part.tx.reinit.prob  = rep(0, 3)
      )
    )
  ),
  ident_max_tx_init = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test + tx (re)init
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(0, 3),
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(0, 3)
      )
    )
  ),
  ident_max_tx_reinit = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test + tx (re)init
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(0, 3),
        part.tx.init.prob    = rep(0, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  ),
  ident_max_tx_both = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test + tx (re)init
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(0, 3),
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  )
)

# Figure 2 ---------------------------------------------------------------------
append_scenario_f2 <- function(sc, name_prefix, sc_fixed,
                               index_inits, partner_idents) {
  sc_cross <- purrr::cross(list(
    "index_inits" = index_inits, "partner_idents" = partner_idents
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed
    sc_param$part.index.prob <- sc_cross[[i]]$index_inits
    sc_param$part.ident.main.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.casl.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.ooff.prob <- sc_cross[[i]]$partner_idents
    scenar <- list(
      list(
        list(
          at = scenarios_update_time,
          param = sc_param
        )
      )
    )

    name <- paste0(
      name_prefix, "__",
      "index__", sc_cross[[i]]$index_inits, "__",
      "partner__", sc_cross[[i]]$partner_idents
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig2 <- append_scenario_f2(
  list(),
  "sc_fig2",
  sc_fixed = list(),
  index_inits = seq(0, 1, length.out = 10),
  partner_idents = seq(0, 1, length.out = 10)
)

# Figure 3 ---------------------------------------------------------------------
append_scenario_f3 <- function(sc, name_prefix, sc_fixed,
                               prep, tx_reinit) {
  sc_cross <- purrr::cross(list(
    "prep" = prep, "tx_reinit" = tx_reinit
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed
    sc_param$part.prep.start.prob <- rep(sc_cross[[i]]$prep, 3)
    sc_param$part.tx.reinit.prob <- rep(sc_cross[[i]]$tx_reinit, 3)

    scenar <- list(
      list(
        list(
          at = scenarios_update_time,
          param = sc_param
        )
      )
    )

    name <- paste0(
      name_prefix, "__",
      "prep__", sc_cross[[i]]$prep, "__",
      "tx_reinit__", sc_cross[[i]]$tx_reinit
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig3 <- append_scenario_f3(
  list(),
  "sc_fig3A",
  sc_fixed = list(),
  prep = seq(0, 1, length.out = 10),
  tx_reinit = seq(0, 1, length.out = 10)
)

sc_fig3 <- append_scenario_f3(
  sc_fig3,
  "sc_fig3B",
  sc_fixed = list(
    part.index.prob = 1,
    part.ident.main.prob = 1,
    part.ident.casl.prob = 1,
    part.ident.ooff.prob = 1,
    part.hiv.test.rate   = rep(1, 3)
  ),
  prep = seq(0, 1, length.out = 10),
  tx_reinit = seq(0, 1, length.out = 10)
)

# Figure 4 ---------------------------------------------------------------------
append_scenario_f4 <- function(sc, name_prefix, sc_fixed,
                               prep, partner_idents) {
  sc_cross <- purrr::cross(list(
    "prep" = prep, "partner_idents" = partner_idents
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed
    sc_param$part.prep.start.prob <- rep(sc_cross[[i]]$prep, 3)
    sc_param$part.ident.main.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.casl.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.ooff.prob <- sc_cross[[i]]$partner_idents

    scenar <- list(
      list(
        list(
          at = scenarios_update_time,
          param = sc_param
        )
      )
    )

    name <- paste0(
      name_prefix, "__",
      "prep__", sc_cross[[i]]$prep, "__",
      "partner__", sc_cross[[i]]$partner_idents
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig4 <- append_scenario_f4(
  list(),
  "sc_fig4A",
  sc_fixed = list(),
  prep = seq(0, 1, length.out = 10),
  partner_idents = seq(0, 1, length.out = 10)
)

sc_fig4 <- append_scenario_f4(
  sc_fig4,
  "sc_fig4B",
  sc_fixed = list(
    part.index.prob = 1,
    part.hiv.test.rate   = rep(1, 3)
  ),
  prep = seq(0, 1, length.out = 10),
  partner_idents = seq(0, 1, length.out = 10)
)

