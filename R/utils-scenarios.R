sc_bases <- list(
  no_ident_no_prep = list(
    at = 52 * 60 + 1,
    param = list(
      part.ident.start = Inf,
      prep.start = Inf,
      riskh.start = Inf # not mandatory but faster
    )
  ),
  no_ident = list(
    at = 52 * 60 + 1,
    param = list(
      part.ident.start = Inf
    )
  ),
  base_atlanta_complete = list()
)

sc_atl_others <- list(
  base_atlanta_missing = list(
    at = 52 * 60 + 1,
    param = list(
      part.hiv.test.rate = rep(0.84, 3),
      part.tx.init.prob = rep(0.96, 3)
    )
  ),
  base_atlanta_complete_alt = list(
    at = 52 * 60 + 1,
    param = list( # Alternate method for ident prob
      part.ident.main.prob = 0.09653631,
      part.ident.casl.prob = 0.09653631,
      part.ident.ooff.prob = 0.04826816
    )
  )
)

sc_ident_max <- list(
  ident_max = list(
    at = 52 * 60 + 1,
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
  ),
  ident_max_test = list(
    at = 52 * 60 + 1,
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
  ),
  ident_max_prep = list(
    at = 52 * 60 + 1,
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
  ),
  ident_max_tx = list(
    at = 52 * 60 + 1,
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

sc_relatives <- list(
  test_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # Part Serv Params
      part.hiv.test.rate   = rep(1, 3)
    )
  ),
  prep_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # Part Serv Params
      part.prep.start.prob = rep(1, 3)
    )
  ),
  test_prep_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # Part Serv Params
      part.hiv.test.rate   = rep(1, 3),
      part.prep.start.prob = rep(1, 3)
    )
  ),
  tx_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # Part Serv Params
      part.tx.init.prob    = rep(1, 3),
      part.tx.reinit.prob  = rep(1, 3)
    )
  ),
  ident_x2  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # see "R/z-indent_prob_calib.R"
      part.ident.main.prob = 2 * param$part.ident.main.prob,
      part.ident.casl.prob = 2 * param$part.ident.casl.prob,
      part.ident.ooff.prob = 2 * param$part.ident.ooff.prob
    )
  ),
  ident_x2_test_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # see "R/z-indent_prob_calib.R"
      part.ident.main.prob = 2 * param$part.ident.main.prob,
      part.ident.casl.prob = 2 * param$part.ident.casl.prob,
      part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
      # Part Serv Params
      part.hiv.test.rate   = rep(1, 3)
    )
  ),
  ident_x2_prep_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # see "R/z-indent_prob_calib.R"
      part.ident.main.prob = 2 * param$part.ident.main.prob,
      part.ident.casl.prob = 2 * param$part.ident.casl.prob,
      part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
      # Part Serv Params
      part.prep.start.prob = rep(1, 3)
    )
  ),
  ident_x2_test_prep_100 = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
      # see "R/z-indent_prob_calib.R"
      part.ident.main.prob = 2 * param$part.ident.main.prob,
      part.ident.casl.prob = 2 * param$part.ident.casl.prob,
      part.ident.ooff.prob = 2 * param$part.ident.ooff.prob,
      # Part Serv Params
      part.hiv.test.rate   = rep(1, 3),
      part.prep.start.prob = rep(1, 3)
    )
  ),
  ident_x2_tx_100  = list(
    at = 52 * 70 + 1,
    param = list(
      part.ident.start = param$prep.start,
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

scenarios <- c(sc_bases, sc_atl_others, sc_ident_max, sc_relatives)
