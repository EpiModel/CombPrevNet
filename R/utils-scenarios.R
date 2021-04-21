scenarios <- list(
  no_ident = list(
    part.ident.start = Inf
  ),
  no_ident_no_prep = list(
    part.ident.start = Inf,
    prep.start = Inf
  ),
  ident_prob1 = list(
    part.ident.start = param$prep.start,
    # Part ident parameters
    part.index.window = 0,
    part.index.degree = 1,
    part.index.prob = 1,
    part.ident.main.window = 52,
    part.ident.casl.window = 52,
    part.ident.ooff.window = 52,
    # see "R/z-indent_prob_calib.R"
    part.ident.main.prob = 1,
    part.ident.casl.prob = 1,
    part.ident.ooff.prob = 1,
    part.hiv.test.rate = rep(1, 3),
    part.prep.start.prob = rep(1, 3),
    part.tx.init.prob = rep(1, 3),
    part.tx.reinit.prob = rep(1, 3)
  ),
  base_atlanta_complete = list(
    part.ident.start = param$prep.start,
    # Part ident parameters
    part.index.window = 0,
    part.index.degree = 1,
    part.index.prob = 0.666,
    part.ident.main.window = 52,
    part.ident.casl.window = 52,
    part.ident.ooff.window = 52,
    # see "R/z-indent_prob_calib.R"
    part.ident.main.prob = 0.13090909,
    part.ident.casl.prob = 0.09000000,
    part.ident.ooff.prob = 0.01484536,
    part.hiv.test.rate = rep(0.394, 3),
    part.prep.start.prob = rep(0, 3),
    part.tx.init.prob = rep(0.387, 3),
    part.tx.reinit.prob = rep(0, 3)
  ),
  base_atlanta_missing = list(
    part.ident.start = param$prep.start,
    # Part ident parameters
    part.index.window = 0,
    part.index.degree = 1,
    part.index.prob = 0.666,
    part.ident.main.window = 52,
    part.ident.casl.window = 52,
    part.ident.ooff.window = 52,
    # see "R/z-indent_prob_calib.R"
    part.ident.main.prob = 0.13090909,
    part.ident.casl.prob = 0.09000000,
    part.ident.ooff.prob = 0.01484536,
    part.hiv.test.rate = rep(0.84, 3),
    part.prep.start.prob = rep(0, 3),
    part.tx.init.prob = rep(0.96, 3),
    part.tx.reinit.prob = rep(0, 3)
  )
)
