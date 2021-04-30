# Utility ----------------------------------------------------------------------
epi_tracker_by_race <- function(ls_funs, races = 1:3,
                                races_names = c("B", "H", "W"),
                                indiv = TRUE, full = TRUE) {

 ls_races <- if (indiv) as.list(races) else list()
 races_names <- if (indiv) races_names else c()

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

epi_tt_traj <- function(traj) {
  function(r_ind) {
    function(dat, at) {
      needed_attributes <- c("race", "tt.traj")
      with(get_attr_list(dat, needed_attributes), {
        sum(race %in% r_ind & tt.traj == traj, na.rm = TRUE)
      })
    }
  }
}

epi_part_ident <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident")
    with(get_attr_list(dat, needed_attributes), {
      sum(race %in% r_ind & part.ident == at, na.rm = TRUE)
    })
  }
}

epi_part_spos <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident", "diag.time")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        part.ident == at &
        diag.time == at,
        na.rm = TRUE)
    })
  }
}

epi_part_sneg <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident", "last.neg.test")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        part.ident == at &
        last.neg.test == at,
        na.rm = TRUE)
    })
  }
}

epi_part_prep <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident", "prepStartTime")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        part.ident == at &
        prepStartTime == at,
        na.rm = TRUE)
    })
  }
}

epi_part_txinit <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident", "part.tx.init.time")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        part.ident == at &
        part.tx.init.time == at,
        na.rm = TRUE)
    })
  }
}

epi_part_txreinit <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "part.ident", "part.tx.reinit.time")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        part.ident == at &
        part.tx.reinit.time == at,
        na.rm = TRUE)
    })
  }
}

epi_ident_dist <- function(n_id, ge = FALSE) {
  function(r_ind) {
    function(dat, at) {
      needed_attributes <- c("race", "part.ident.counter")
      with(get_attr_list(dat, needed_attributes), {
        if (n_id == 0) {
          sum(
            race %in% r_ind &
            (is.na(part.ident.counter) | part.ident.counter == n_id),
            na.rm = TRUE
          )
        } else if (ge) {
          sum(race %in% r_ind & part.ident.counter >= n_id, na.rm = TRUE)
        } else {
          sum(race %in% r_ind & part.ident.counter == n_id, na.rm = TRUE)
        }
      })
    }
  }
}

epi_partner_count <- function(rel_type) {
  function(dat, at) {
    function(dat, at) {
      plist <- dat$temp$plist
      plist <- dat$temp$plist
      plist <- plist[plist[, 3] == rel_type, ]
      plist <- plist[plist[, 3] == rel_type, ]
      mean(table(plist[, c(1, 2)]))
      mean(table(plist[, c(1, 2)]))
    }
  }
}

epi_prep_start <- function(r_ind) {
  function(dat, at) {
    needed_attributes <- c("race", "prepStartTime")
    with(get_attr_list(dat, needed_attributes), {
      sum(
        race %in% r_ind &
        prepStartTime == at,
        na.rm = TRUE)
    })
  }
}
