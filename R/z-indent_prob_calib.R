library(dplyr)
library(tidyr)

if (!file.exists("out/partdist.rds")) {
  nsims <- ncores <- 3
  lnt <- TRUE
  source("R/utils-params.R")
  orig <- readRDS("out/est/restart.rds")
  orig$attr[[1]]$part.ident.counter <- rep(NA, length(orig$attr[[1]]$part.ident))
  orig$attr[[1]]$prep.start.counter <- rep(NA, length(orig$attr[[1]]$part.ident))

  param$epi_trackers <- list()
  param$part.index.prob <- 1

  control <- control_msm(
    simno = 1,
    start = 60 * 52 + 1,
    nsteps = 100 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
    nsims = ncores,
    ncores = ncores,
    initialize.FUN = reinit_msm,
    save.nwstats = FALSE,
    # raw.output = TRUE,
    raw.output = TRUE,
    verbose = FALSE
  )

  # Ensure this is in mod.partident
  #
  # after this line:
  #
  #  plist.temp <- plist.temp[p1_found | p2_found, , drop = FALSE]

  ##################################################################################
  ##  if (is.null(dat$sav_ident))
  ##    dat$sav_ident <- list()
  ##
  ##  elt <- list(
  ##    at = at,
  ##    found_uids = found.uid,
  ##    plist = plist.temp[, 1:3, drop = FALSE]
  ##  )
  ##
  ##  dat$sav_ident <- append(dat$sav_ident, list(elt))
  ##
  ##
  ##################################################################################

  ## Simulation
  # debug(partident_msm)
  sim <- netsim(orig, param, init, control)

  pl <- list()
  for (i in seq_along(sim))
    pl <- c(pl, sim[[i]]$sav_ident)

  bp <- vector(mode = "list", length = length(pl))

  for (i in seq_along(pl)) {
    fu <- pl[[i]][["found_uids"]]
    p <- pl[[i]][["plist"]]

    p[, 1:2][! p[, 1:2] %in% fu ] <- NA
    # If NA in `p1`, put `p2`. `p1` now only contains indexes
    p[, "p1"] <- ifelse(is.na(p[, "p1"]), p[, "p2"], p[, "p1"])
    # In case of a partnership where both are identified, `p1` is still NA.
    # We also remove `p2` which is now useless
    p <- p[!is.na(p[, "p1"]), -2, drop = FALSE]

    nfu <- fu[!fu %in% p[,1]]
    if (length(nfu) > 0) {
      p <-  rbind(p, matrix(c(nfu, rep(NA, length(nfu))), ncol = 2))
    }
    bp[[i]] <- p
  }

  bp <- lapply(bp, as_tibble)
  bp <- bind_rows(bp)

  final <- bp %>%
    group_by(p1, ptype) %>%
    summarise(n = n()) %>%
    mutate(ptype = paste0("ptype_", ptype)) %>%
    pivot_wider(names_from = ptype, values_from = n, values_fill = 0) %>%
    mutate(total = ptype_1 + ptype_2 + ptype_3)

  saveRDS(final, "out/partdist.rds")
} else {
  final <- readRDS("out/partdist.rds")
}

# Mean number of partnership per type
x <- colMeans(final[, 2:4])
# 0.94 1.4 9.5
target_ident <- 0.432

# "221" "421" "all_eq"
strategy <- "421"

if (strategy == "221") {
  ## option 1 - we enforce part.indent.main.prob == part.indent.casl.prob == 2 * part.indent.oof.prob
  ratio <- c(2, 2, 1)
  p_temp <- target_ident / sum(x * ratio)
  p_temp
  # [1] 0.02689713
  p <- p_temp * ratio
  print(p)
  # [1] 0.06090590 0.06090590 0.03045295
  print(p * x)
  # [1] 0.05711067 0.08421622 0.29067311
  sum(p * x)
  # [1] 0.432
} else if (strategy == "421") {
  ## option 1 - we enforce part.indent.main.prob == 2 * part.indent.casl.prob == 4 * part.indent.oof.prob
  ##
  ratio <- c(4, 2, 1)
  p_temp <- target_ident / sum(x * ratio)
  p_temp
  # [1] 0.02689713
  p <- p_temp * ratio
  print(p)
  # [1] 0.10758853 0.05379427 0.02689713
  print(p * x)
  # [1] 0.10088436 0.07438277 0.25673287
  sum(p * x)
  # [1] 0.432
} else if (strategy == "all_eq") {
  ## option 2
  ##
  ## we force each partnership type to give the same "number" of partners
  p <- target_ident / 3 / x
  print(p)
  # [1] 0.2057143 0.1440000 0.0240000
  print(p * x)
  # [1] 0.144 0.144 0.144# validation
  sum(p * x)
  # [1] 0.432
}


final <- mutate(
  final,
  total_ident =
    rbinom(length(p1), ptype_1, p[1]) +
    rbinom(length(p1), ptype_2, p[2]) +
    rbinom(length(p1), ptype_3, p[3])
)

mean(final$total_ident)

table(final$total_ident) %>%
  prop.table() %>%
  round(4)

# CDC data - mean is 0.9, just use to get the shape of the distribution
part <- c(
  NA, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 35, 36, 40, 47, 49, 54, 60, 65, 75
)

n <- c(
  1949, 14728, 10502, 2952, 1223, 566, 317, 177, 113, 74, 51, 36, 30, 25, 12,
  15, 12, 6, 4, 7, 4, 5, 7, 4, 4, 4, 4, 1, 2, 3, 3, 3, 1, 2, 1, 2, 2, 1, 1, 1,
  1, 1
)

serie <- c()
for (i in seq_along(n))
  serie <- c(serie, rep(part[i], n[i]))

table(serie) %>%
  prop.table() %>%
  round(4)

