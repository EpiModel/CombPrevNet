main_prob_50 <- 0.66 # main partner_ident prob to get to 50% overall
main_prob_25 <- 0.370  # main partner_ident prob to get to 25% overall
contour_length <- 16

# Scenarios tables =============================================================
# Utilities
make_increments <- function(param, increments) {
  lapply(increments, function(x) param * x)
}

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

scenarios_update_time <- 52 * 70 + 1

increments <- c(1.1, 1.50, 2.0)
increments_names <- scales::number_format(0.01)(increments)

absolutes <- c(10, 25, 50, 75, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

windows <- c(26, 4)
windows_names <- stringr::str_pad(windows, 2, "left", "0")

degrees <- c(2, 3, 5, 10)
degrees_names <- stringr::str_pad(degrees, 2, "left", "0")

# Base Scenario ----------------------------------------------------------------
sc_base <- list(base_atlanta_complete = list())
sc_atlanta_missing <- list(
  base_atlanta_missing = list(
    list(
      at = scenarios_update_time,
      param = list(
        part.hiv.test.rate   = rep(0.84, 3),
        part.tx.init.prob    = rep(0.96, 3)
      )
    )
  )
)

# Table 2 ----------------------------------------------------------------------
sc_t2 <- list()

# Index initiation
sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_index_init_a", stringr::str_pad(c(7:10) * 10, 3, "left", 0)),
  list(
    part.index.prob = list(0.7, 0.8, 0.9, 1)
  )
)


# Partner Identification
increments <- c(1.1, 1.5)
increments_names <- scales::number_format(0.01)(increments)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_part_ident_i", increments_names),
  list(
    part.ident.main.prob = make_increments(param$part.ident.main.prob, increments),
    part.ident.casl.prob = make_increments(param$part.ident.casl.prob, increments),
    part.ident.ooff.prob = make_increments(param$part.ident.ooff.prob, increments)
  )
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

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
increments <- c(1.1, 1.5, 2)
increments_names <- scales::number_format(0.01)(increments)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_hiv_test_i", increments_names),
  list(
    part.hiv.test.rate = make_increments(param$part.hiv.test.rate, increments)
  )
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_hiv_test_a", absolutes_names),
  list(
    part.hiv.test.rate = lapply(absolutes, rep, 3)
  )
)

# PrEP linkage
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_prep_start_a", absolutes_names),
  list(
    part.prep.start.prob = lapply(absolutes, rep, 3)
  )
)

# Tx Init
increments <- c(1.1, 1.5)
increments_names <- scales::number_format(0.01)(increments)

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_init_i", increments_names),
  list(
    part.tx.init.prob = make_increments(param$part.tx.init.prob, increments)
  )
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_init_a", absolutes_names),
  list(
    part.tx.init.prob = lapply(absolutes, rep, 3)
  )
)

# Tx Reinit
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t2 <- append_scenario_seq(
  sc_t2,
  paste0("t2_tx_reinit_a", absolutes_names),
  list(
    part.tx.reinit.prob = lapply(absolutes, rep, 3)
  )
)

# Table 3a ---------------------------------------------------------------------
sc_t3a <- list()

t3a_base <- function(n) {
  list(
    part.index.prob = as.list(rep(0.9, n)),
    part.ident.main.prob = as.list(rep(main_prob_25, n)),
    part.ident.casl.prob = as.list(rep(plogis(qlogis(main_prob_25) - log(2)), n)),
    part.ident.ooff.prob = as.list(rep(plogis(qlogis(main_prob_25) - log(4)), n))
  )
}

# Testing
increments <- c(1.1, 1.5, 2)
increments_names <- scales::number_format(0.01)(increments)

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_hiv_test_i", increments_names),
  c(t3a_base(length(increments)), list(
    part.hiv.test.rate = make_increments(param$part.hiv.test.rate, increments)
  ))
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_hiv_test_a", absolutes_names),
  c(t3a_base(length(absolutes)), list(
    part.hiv.test.rate = lapply(absolutes, rep, 3)
  ))
)

# PrEP linkage
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_prep_start_a", absolutes_names),
  c(t3a_base(length(absolutes)), list(
    part.prep.start.prob = lapply(absolutes, rep, 3)
  ))
)

# Tx Init
increments <- c(1.1, 1.5)
increments_names <- scales::number_format(0.01)(increments)

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_tx_init_i", increments_names),
  c(t3a_base(length(increments)), list(
    part.tx.init.prob = make_increments(param$part.tx.init.prob, increments)
  ))
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_tx_init_a", absolutes_names),
  c(t3a_base(length(absolutes)), list(
    part.tx.init.prob = lapply(absolutes, rep, 3)
  ))
)

# Tx Reinit
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3a <- append_scenario_seq(
  sc_t3a,
  paste0("t3a_tx_reinit_a", absolutes_names),
  c(t3a_base(length(absolutes)), list(
    part.tx.reinit.prob = lapply(absolutes, rep, 3)
  ))
)

# Table 3a ---------------------------------------------------------------------
sc_t3b <- list()

t3b_base <- function(n) {
  list(
    part.index.prob = as.list(rep(0.9, n)),
    part.ident.main.prob = as.list(rep(main_prob_50, n)),
    part.ident.casl.prob = as.list(rep(plogis(qlogis(main_prob_50) - log(2)), n)),
    part.ident.ooff.prob = as.list(rep(plogis(qlogis(main_prob_50) - log(4)), n))
  )
}

# Testing
increments <- c(1.1, 1.5, 2)
increments_names <- scales::number_format(0.01)(increments)

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_hiv_test_i", increments_names),
  c(t3b_base(length(increments)), list(
    part.hiv.test.rate = make_increments(param$part.hiv.test.rate, increments)
  ))
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_hiv_test_a", absolutes_names),
  c(t3b_base(length(absolutes)), list(
    part.hiv.test.rate = lapply(absolutes, rep, 3)
  ))
)

# PrEP linkage
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_prep_start_a", absolutes_names),
  c(t3b_base(length(absolutes)), list(
    part.prep.start.prob = lapply(absolutes, rep, 3)
  ))
)

# Tx Init
increments <- c(1.1, 1.5)
increments_names <- scales::number_format(0.01)(increments)

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_tx_init_i", increments_names),
  c(t3b_base(length(increments)), list(
    part.tx.init.prob = make_increments(param$part.tx.init.prob, increments)
  ))
)

absolutes <- c(100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_tx_init_a", absolutes_names),
  c(t3b_base(length(absolutes)), list(
    part.tx.init.prob = lapply(absolutes, rep, 3)
  ))
)

# Tx Reinit
absolutes <- c(10, 25, 50, 100)
absolutes_names <- stringr::str_pad(absolutes, 3, "left", "0")
absolutes <- absolutes / 100

sc_t3b <- append_scenario_seq(
  sc_t3b,
  paste0("t3b_tx_reinit_a", absolutes_names),
  c(t3b_base(length(absolutes)), list(
    part.tx.reinit.prob = lapply(absolutes, rep, 3)
  ))
)

# Table 4 ----------------------------------------------------------------------
sc_t4 <- list(
  # t4_no_ident_no_prep = list(
  #   list(
  #     at = param$riskh.start - 1,
  #     param = list(
  #       part.ident.start = Inf,
  #       prep.start = Inf,
  #       riskh.start = Inf # not mandatory but faster
  #     )
  #   )
  # ),
  t4_no_ident = list(
    list(
      at = param$riskh.start - 1,
      param = list(
        part.ident.start = Inf
      )
    )
  ),
  t4_ident_max_ident = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test (prep effect via LNT)
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1
        # Part Serv Params
      )
    )
  ),
  t4_ident_max_test = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test (prep effect via LNT)
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 1,
        part.ident.main.prob = 1,
        part.ident.casl.prob = 1,
        part.ident.ooff.prob = 1,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3)
      )
    )
  ),
  t4_ident_max_prep = list(
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
        part.prep.start.prob = rep(1, 3)
      )
    )
  ),
  t4_ident_max_tx_init = list(
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
        part.tx.init.prob    = rep(1, 3)
      )
    )
  ),
  t4_ident_max_tx_reinit = list(
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
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  ),
  t4_ident_max_tx_both = list(
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
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  ),
  t4_ident_max_all = list(
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
  )
)

# Table 5a ----------------------------------------------------------------------
sc_t5a_base <- list(
  t5a_base = list(
    list(
      at = scenarios_update_time,
      param = list( # maximum test (prep effect via LNT)
        # see "R/z-indent_prob_calib.R"
        part.index.prob = 0.9,
        part.ident.main.prob = main_prob_50,
        part.ident.casl.prob = plogis(qlogis(main_prob_50) - log(2)),
        part.ident.ooff.prob = plogis(qlogis(main_prob_50) - log(4))
      )
    )
  )
)

# windows
sc_t5a <- list()
t5a_base <- t3b_base

sc_t5a <- append_scenario_seq(
  sc_t5a,
  paste0("t5a_main_win_a", windows_names),
  c(t5a_base(length(windows)), list(
    part.ident.main.window = as.list(windows)
  ))
)

sc_t5a <- append_scenario_seq(
  sc_t5a,
  paste0("t5a_casl_win_a", windows_names),
  c(t5a_base(length(windows)), list(
    part.ident.casl.window = as.list(windows)
  ))
)

sc_t5a <- append_scenario_seq(
  sc_t5a,
  paste0("t5a_ooff_win_a", windows_names),
  c(t5a_base(length(windows)), list(
    part.ident.ooff.window = as.list(windows)
  ))
)

# degrees
sc_t5a <- append_scenario_seq(
  sc_t5a,
  paste0("t5a_index_degree_a", degrees_names),
  c(t5a_base(length(degrees)), list(
    part.index.degree = as.list(degrees)
  ))
)

# Table 5b ----------------------------------------------------------------------
# windows
sc_t5b <- list()
t5b_base <- function(n) {
  list(
    part.index.prob = as.list(rep(1, n)),
    part.ident.main.prob = as.list(rep(1, n)),
    part.ident.casl.prob = as.list(rep(1, n)),
    part.ident.ooff.prob = as.list(rep(1, n)),
    # Part Serv Params
    part.hiv.test.rate   = replicate(n, rep(1, 3), simplify = FALSE),
    part.prep.start.prob = replicate(n, rep(1, 3), simplify = FALSE),
    part.tx.init.prob    = replicate(n, rep(1, 3), simplify = FALSE),
    part.tx.reinit.prob  = replicate(n, rep(1, 3), simplify = FALSE)
  )
}

sc_t5b <- append_scenario_seq(
  sc_t5b,
  paste0("t5b_main_win_a", windows_names),
  c(t5b_base(length(windows)), list(
    part.ident.main.window = as.list(windows)
  ))
)

sc_t5b <- append_scenario_seq(
  sc_t5b,
  paste0("t5b_casl_win_a", windows_names),
  c(t5b_base(length(windows)), list(
    part.ident.casl.window = as.list(windows)
  ))
)

sc_t5b <- append_scenario_seq(
  sc_t5b,
  paste0("t5b_ooff_win_a", windows_names),
  c(t5b_base(length(windows)), list(
    part.ident.ooff.window = as.list(windows)
  ))
)

# degrees
sc_t5b <- append_scenario_seq(
  sc_t5b,
  paste0("t5b_index_degree_a", degrees_names),
  c(t5b_base(length(degrees)), list(
    part.index.degree = as.list(degrees)
  ))
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
  "sc_fig2A",
  sc_fixed = list(),
  index_inits = seq(0, 1, length.out = contour_length),
  partner_idents = seq(0, 1, length.out = contour_length)
)

sc_fig2 <- append_scenario_f2(
  sc_fig2,
  "sc_fig2B",
  sc_fixed = list(
    part.hiv.test.rate   = rep(1, 3),
    part.prep.start.prob = rep(1, 3),
    part.tx.init.prob    = rep(1, 3),
    part.tx.reinit.prob  = rep(1, 3)
  ),
  index_inits = seq(0, 1, length.out = contour_length),
  partner_idents = seq(0, 1, length.out = contour_length)
)

# Figure 3 ---------------------------------------------------------------------
append_scenario_f3 <- function(sc, name_prefix, sc_fixed,
                               prep, test) {
  sc_cross <- purrr::cross(list(
    "prep" = prep, "test" = test
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed
    sc_param$part.prep.start.prob <- rep(sc_cross[[i]]$prep, 3)
    sc_param$part.hiv.test.rate <- rep(sc_cross[[i]]$test, 3)

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
      "test__", sc_cross[[i]]$test
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig3 <- append_scenario_f3(
  list(),
  "sc_fig3A",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.ident.main.prob = main_prob_25,
    part.ident.casl.prob = plogis(qlogis(main_prob_25) - log(2)),
    part.ident.ooff.prob = plogis(qlogis(main_prob_25) - log(4))
  ),
  prep = seq(0, 1, length.out = contour_length),
  test = seq(0, 1, length.out = contour_length)
)

sc_fig3 <- append_scenario_f3(
  sc_fig3,
  "sc_fig3B",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.ident.main.prob = main_prob_50,
    part.ident.casl.prob = plogis(qlogis(main_prob_50) - log(2)),
    part.ident.ooff.prob = plogis(qlogis(main_prob_50) - log(4))
  ),
  prep = seq(0, 1, length.out = contour_length),
  test = seq(0, 1, length.out = contour_length)
)

# Figure 4 ---------------------------------------------------------------------
append_scenario_f4 <- function(sc, name_prefix, sc_fixed,
                               tx, test) {
  sc_cross <- purrr::cross(list(
    "tx" = tx, "test" = test
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed

    sc_param$part.tx.init.prob <- rep(sc_cross[[i]]$tx, 3)
    sc_param$part.tx.reinit.prob <- rep(sc_cross[[i]]$tx, 3)
    sc_param$part.hiv.test.rate <- rep(sc_cross[[i]]$test, 3)

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
      "tx__", sc_cross[[i]]$tx, "__",
      "test__", sc_cross[[i]]$test
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig4 <- append_scenario_f4(
  list(),
  "sc_fig4A",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.ident.main.prob = main_prob_25,
    part.ident.casl.prob = plogis(qlogis(main_prob_25) - log(2)),
    part.ident.ooff.prob = plogis(qlogis(main_prob_25) - log(4))
  ),
  tx = seq(0, 1, length.out = contour_length),
  test = seq(0, 1, length.out = contour_length)
)

sc_fig4 <- append_scenario_f4(
  sc_fig4,
  "sc_fig4B",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.ident.main.prob = main_prob_50,
    part.ident.casl.prob = plogis(qlogis(main_prob_50) - log(2)),
    part.ident.ooff.prob = plogis(qlogis(main_prob_50) - log(4))
  ),
  tx = seq(0, 1, length.out = contour_length),
  test = seq(0, 1, length.out = contour_length)
)

# Figure 5 ---------------------------------------------------------------------
append_scenario_f5 <- function(sc, name_prefix, sc_fixed,
                               partner_idents, service, type) {
  sc_cross <- purrr::cross(list(
    "partner_idents" = partner_idents, "service" = service
  ))

  for (i in seq_along(sc_cross)) {
    sc_param <- sc_fixed

    sc_param$part.ident.main.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.casl.prob <- sc_cross[[i]]$partner_idents
    sc_param$part.ident.ooff.prob <- sc_cross[[i]]$partner_idents

    if (type == "prep") {
      sc_param$part.prep.start.prob <- rep(sc_cross[[i]]$service, 3)
    } else if (type == "tx") {
      sc_param$part.tx.init.prob <- rep(sc_cross[[i]]$service, 3)
      sc_param$part.tx.reinit.prob <- rep(sc_cross[[i]]$service, 3)
    } else {
      stop("Type must be either 'prep' or 'tx'")
    }


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
      "partner_idents__", sc_cross[[i]]$partner_idents, "__",
      "service__", sc_cross[[i]]$service
    )
    names(scenar) <- name

    sc <- c(sc, scenar)
  }

  sc
}

sc_fig5 <- append_scenario_f5(
  list(),
  "sc_fig5A",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.hiv.test.rate   = rep(1, 3)
  ),
  partner_idents = seq(0, 1, length.out = contour_length),
  service = seq(0, 1, length.out = contour_length),
  type = "prep"
)

sc_fig5 <- append_scenario_f5(
  sc_fig5,
  "sc_fig5B",
  sc_fixed = list(
    part.index.prob = 0.9,
    part.hiv.test.rate   = rep(1, 3)
  ),
  partner_idents = seq(0, 1, length.out = contour_length),
  service = seq(0, 1, length.out = contour_length),
  type = "tx"
)
