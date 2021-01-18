# Utility ----------------------------------------------------------------------
epi_tracker_by_race <- function(ls_funs, races = 1:3,
                                races_names = c("B", "H", "W"),
                                indiv = TRUE, full = TRUE) {

 ls_races <- if (indiv) as.list(races) else list()

  if (full) {
    ls_races <- c(ls_races, list(races))
    races_names <- c(races_names, "ALL")
  }

  epi_tracker <- lapply(
    ls_races,
    function(race) lapply(ls_funs, do.call, args = list(r_ind = race))
  )

  epi_tracker <- unlist(epi_tracker)
  names(epi_tracker) <- paste0(
    names(epi_tracker), "___",
    unlist(lapply(races_names, rep, times = length(ls_funs)))
  )

  epi_tracker
}

# Trackers ---------------------------------------------------------------------
epi_s <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 0, na.rm = TRUE)
    })
  }
}

epi_s_prep_elig <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "prepElig")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 0 & prepElig == 1, na.rm = TRUE)
    })
  }
}

epi_s_prep <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "prepStat")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 0 & prepStat == 1, na.rm = TRUE)
    })
  }
}

epi_i <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 1, na.rm = TRUE)
    })
  }
}

epi_i_dx <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "diag.status")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 1 & diag.status == 1, na.rm = TRUE)
    })
  }
}

epi_i_tx <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "tx.status")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 1 & tx.status == 1, na.rm = TRUE)
    })
  }
}

epi_i_sup <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "vl.last.supp")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & status == 1 & vl.last.supp == at, na.rm = TRUE)
    })
  }
}

epi_i_sup_dur <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "status", "vl.last.usupp")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind &
          status == 1 &
          at - vl.last.usupp >= 52,
          na.rm = TRUE)
    })
  }
}
