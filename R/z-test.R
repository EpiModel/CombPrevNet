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

ident_probsA <- c(0.09191489, 0.09191489, 0.04595745)
ident_probsB <- c(0.2057143, 0.1440000, 0.0240000)
cc <- c(0.24000000, 0.16000000, 0.01515789)
pp <- ident_probsB #* 1.5

pp <- c(0.4, 0.06, 0.04)
final <- mutate(
  final,
  total_ident =
    rbinom(length(p1), ptype_1, pp[1]) +
    rbinom(length(p1), ptype_2, pp[2]) +
    rbinom(length(p1), ptype_3, pp[3])
)

mean(final$total_ident)
table(final$total_ident) %>%
  prop.table() %>% round(4)


# CDC data
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

table(serie) %>% prop.table()  %>% round(4)

found <- sum(part * n, na.rm = T)
n_tot <- sum(n[-1])
found / n_tot
scales::label_percent()(n[-1] / n_tot)

hist(serie)
hist(rpois(sum(n[-1]), 0.9806))


# EasyABC

model <- function(pp) {
  ff <- mutate(
    final,
    total_ident =
      rbinom(length(p1), ptype_1, pp[1]) +
      rbinom(length(p1), ptype_2, pp[2]) +
      rbinom(length(p1), ptype_3, pp[3])
  )

  x1 <- table(serie) %>% prop.table()
  dx1 <- tibble(n = as.numeric(names(x1)), p = as.numeric(x1))
  x2 <- table(ff$total_ident) %>% prop.table()
  dx2 <- tibble(n = as.numeric(names(x2)), p = as.numeric(x2))

  dx <- full_join(dx1, dx2, by = "n") %>%
    replace_na(list(p.x = 0, p.y = 0))

  miss_n <- setdiff(0:75, dx$n)
  dx_miss <- tibble(n = miss_n, p.x = 0, p.y = 0)
  dx <- bind_rows(dx, dx_miss) %>% arrange(n)

  suppressMessages(philentropy::KL(t(dx[, 2:3])))
}

EasyABC::ABC_sequential(
  method = "Lenormand",
  model,
  prior = list(c("unif", 0, 1), c("unif", 0, 1), c("unif", 0, 1)),
  prior_test = "X1>0 & X2>0 & X3 > 0 & X1<1 & X2<1 & X3<1",
  nb_simul = 20,
  summary_stat_target = 0,
  p_acc_min = 0.1
)


EasyABC::ABC_sequential(
  method = "Drovandi",
  model,
  prior = list(c("unif", 0, 1), c("unif", 0, 1), c("unif", 0, 1)),
  prior_test = "X1>0 & X2>0 & X3 > 0 & X1<1 & X2<1 & X3<1",
  nb_simul = 20,
  summary_stat_target = 0,
  tolerance_tab = 0.05,
  c_drov = 0.7
)

EasyABC::ABC_sequential(
  method = "Delmoral",
  model,
  prior = list(c("unif", 0, 1), c("unif", 0, 1), c("unif", 0, 1)),
  prior_test = "X1>0 & X2>0 & X3 > 0 & X1<1 & X2<1 & X3<1",
  nb_simul = 20,
  summary_stat_target = 0,
  tolerance_target = 0.05,
  alpha = 0.5
)
