# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "ir100"              = "Incidence Rate",
  "nia"                = "NIA",
  "pia"                = "PIA",
  "nnt"                = "NNT",
  "cum_incid"          = "Cumulative incidence",
  "hiv_prev"           = "HIV Prevalence",
  # Process
  "prep_cov"           = "PrEP Coverage",
  "hiv_diag"           = "HIV+ Diagnosed",
  "hiv_tx"             = "HIV+ Treated",
  "hiv_supp"           = "HIV+ Virally Suppressed",
  "prep_start"         = "Total Number of individuals who Started PrEP",
  # Part Process
  "elig_indexes"       = "Number of Eligible Indexes",
  "found_indexes"      = "Number of Indexes Found",
  "prop_found_indexes" = "Proportion of Indexes Identified",
  "elig_partners"       = "Number of Eligible Partners",
  "found_partners"      = "Number of Identified Partners",
  "prop_found_partners" = "Proportion of Partners Identified",
  "partners_found_per_indexes" = "Number of Partners Identified Per Index",
  # "part_ident"         = "Number of Identified Partners",
  "part_screened"      = "Number of Screened Partners",
  "part_sneg"          = "Number of Screened Partners (neg)",
  "part_spos"          = "Number of Screened Partners (pos)",
  "part_prep"          = "Number of Partners who Started PrEP",
  "part_txinit"        = "Number of Partners who Started ART",
  "part_txreinit"      = "Number of Partners who Restarted ART",
  "ident_dist0"        = "Identified Distribution: 0",
  "ident_dist1"        = "Identified Distribution: 1",
  "ident_dist2"        = "Identified Distribution: 2",
  "ident_dist3p"       = "Identified Distribution: 3+",

  "prep_time_on"  = "prep_time_on",
  "prep_episodes" = "prep_episodes"
)

figure1_labels <- c(
  "base_atlanta_complete"  = "Atlanta Complete",
  "t4_no_ident"            = "No Partner Services",
  "t4_ident_max_ident"     = "Max ID",
  "t4_ident_max_test"      = "Max ID + Test",
  "t4_ident_max_prep"      = "Max ID + Test + PrEP",
  "t4_ident_max_tx_init"   = "Max ID + Test + tx init",
  "t4_ident_max_tx_reinit" = "Max ID + Test + tx reinit",
  "t4_ident_max_tx_both"   = "Max ID + Test + tx init/reinit",
  "t4_ident_max_all"       = "Max all"
)

figure2_panel <- c(
  "sc_fig2A" = "Base partner services",
  "sc_fig2B" = "Max partner services"
)

figure3_panel <- c(
  "sc_fig3A" = "Index prob 90%, Partner prob 25%",
  "sc_fig3B" = "Index prob 90%, Partner prob 50%"
)

figure4_panel <- figure3_panel

# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)
fmts[["ir100"]] <- scales::label_number(0.01)
fmts[["nnt"]] <- scales::label_number(0.01)
fmts[["pia"]] <- scales::label_percent(0.1)
fmts[["prep_cov"]] <- scales::label_percent(0.1)
fmts[["hiv_prev"]] <- scales::label_percent(0.1)
fmts[["hiv_diag"]] <- scales::label_percent(0.1)
fmts[["hiv_tx"]] <- scales::label_percent(0.1)
fmts[["hiv_supp"]] <- scales::label_percent(0.1)
fmts[["ident_dist0"]] <- scales::label_percent(1)
fmts[["ident_dist1"]] <- scales::label_percent(0.001)
fmts[["ident_dist2"]] <- scales::label_percent(0.001)
fmts[["ident_dist3p"]] <- scales::label_percent(0.001)
fmts[["prop_found_indexes"]] <- scales::label_percent(0.1)
fmts[["prop_found_partners"]] <- scales::label_percent(0.1)
fmts[["partners_found_per_indexes"]] <- scales::label_number(0.001)

make_ordered_labels <- function(nms, named_labels) {
  ordered_labels <- named_labels[nms]
  ordered_labels <- paste0(seq_along(ordered_labels), "-", ordered_labels)
  names(ordered_labels) <- nms

  ordered_labels
}
