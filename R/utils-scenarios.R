scenarios_update_time <- 52 * 60 + 1

sc_bases <- list(
  no_ident_no_prep = list(
    list(
      at = scenarios_update_time,
      param = list(
        part.ident.start = Inf,
        prep.start = Inf,
        riskh.start = Inf # not mandatory but faster
      )
    )
  ),
  no_ident = list(
    list(
      at = scenarios_update_time,
      param = list(
        part.ident.start = Inf
      )
    )
  ),
  base_atlanta_complete = list()
)

sc_atl_others <- list(
  base_atlanta_missing = list(
    list(
      at = scenarios_update_time,
      param = list(
        part.hiv.test.rate = rep(0.84, 3),
        part.tx.init.prob = rep(0.96, 3)
      )
    )
  ),
  base_atlanta_complete_alt = list(
    list(
      at = scenarios_update_time,
      param = list( # Alternate method for ident prob
        part.ident.main.prob = 0.09191489,
        part.ident.casl.prob = 0.09191489,
        part.ident.ooff.prob = 0.04595745
      )
    )
  )
)

sc_relatives <- list(
  test_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3)
      )
    )
  ),
  prep_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # Part Serv Params
        part.prep.start.prob = rep(1, 3)
      )
    )
  ),
  test_prep_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(1, 3)
      )
    )
  ),
  tx_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # Part Serv Params
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  ),
  ident_x2  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # see "R/z-indent_prob_calib.R"
        part.ident.main.prob = 2 * param$part.ident.main.prob,
        part.ident.casl.prob = 2 * param$part.ident.casl.prob,
        part.ident.ooff.prob = 2 * param$part.ident.ooff.prob
      )
    )
  ),
  ident_x2_test_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # see "R/z-indent_prob_calib.R"
        part.ident.main.prob = 2 * param$part.ident.main.prob,
        part.ident.casl.prob = 2 * param$part.ident.casl.prob,
        part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3)
      )
    )
  ),
  ident_x2_prep_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # see "R/z-indent_prob_calib.R"
        part.ident.main.prob = 2 * param$part.ident.main.prob,
        part.ident.casl.prob = 2 * param$part.ident.casl.prob,
        part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
        # Part Serv Params
        part.prep.start.prob = rep(1, 3)
      )
    )
  ),
  ident_x2_test_prep_100 = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # see "R/z-indent_prob_calib.R"
        part.ident.main.prob = 2 * param$part.ident.main.prob,
        part.ident.casl.prob = 2 * param$part.ident.casl.prob,
        part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
        # Part Serv Params
        part.hiv.test.rate   = rep(1, 3),
        part.prep.start.prob = rep(1, 3)
      )
    )
  ),
  ident_x2_tx_100  = list(
    list(
      at = 52 * 70 + 1,
      param = list(
        # see "R/z-indent_prob_calib.R"
        part.ident.main.prob = 2 * param$part.ident.main.prob,
        part.ident.casl.prob = 2 * param$part.ident.casl.prob,
        part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
        # Part Serv Params
        part.tx.init.prob    = rep(1, 3),
        part.tx.reinit.prob  = rep(1, 3)
      )
    )
  )
)

sc_ident_max <- list(
  ident_max = list(
    list(
      at = 52 * 70 + 1,
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
      at = 52 * 70 + 1,
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
      at = 52 * 70 + 1,
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
  ident_max_tx = list(
    list(
      at = 52 * 70 + 1,
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

t2 <- list()
t2 <- append_scenario_seq(
  t2,
  paste0("t2_index_init_", stringr::str_pad(c(7:10), 2, "left", 0)),
  list(
    part.index.prob = list(0.7, 0.8, 0.9, 1)
  )
)

increments <- c(1.15, 1.25, 1.50)
increments_names <- c(15, 25, 50)
t2 <- append_scenario_seq(
  t2,
  paste0("t2_part_ident_i", increments_names),
  list(
    part.ident.main.prob = as.list(param$part.ident.main.prob * increments),
    part.ident.casl.prob = as.list(param$part.ident.casl.prob * increments),
    part.ident.ooff.prob = as.list(param$part.ident.ooff.prob * increments)
  )
)

t2 <- append_scenario_seq(
  t2,
  paste0("t2_hiv_test_i", increments_names),
  list(
    part.hiv.test.rate = lapply(param$part.hiv.test.rate * increments, rep, 3),
  )
)

# WARNING, MAKES NO SENS AS INITIAL VALUE IS 0
t2 <- append_scenario_seq(
  t2,
  paste0("t2_prep_start_i", increments_names),
  list(
    part.prep.start.prob = lapply(
      param$part.prep.start.prob * increments,
      rep, 3
    ),
  )
)

t2 <- append_scenario_seq(
  t2,
  paste0("t2_tx_init_i", increments_names),
  list(
    part.tx.init.prob = lapply(param$part.tx.init.prob * increments, rep, 3),
  )
)

t2 <- append_scenario_seq(
  t2,
  paste0("t2_tx_reinit_i", increments_names),
  list(
    part.tx.reinit.prob = lapply(param$part.tx.reinit.prob * increments, rep, 3),
  )
)

windows <- c(26, 4)
t3 <- list()
t3 <- append_scenario_seq(
  t3,
  paste0("t3_main_win_", windows),
  list(
    part.ident.main.window = as.list(windows)
  )
)

t3 <- append_scenario_seq(
  t3,
  paste0("t3_cals_win_", windows),
  list(
    part.ident.casl = as.list(windows)
  )
)

t3 <- append_scenario_seq(
  t3,
  paste0("t3_ooff_win_", windows),
  list(
    part.ident.ooff = as.list(windows)
  )
)

degrees <- c(1, 2, 3, 5, 10)
t3 <- append_scenario_seq(
  t3,
  paste0("t3_index_degree_", degrees),
  list(
    part.index.degree = as.list(degrees)
  )
)

scenarios <- c(sc_bases, sc_atl_others, sc_ident_max, sc_relatives)

