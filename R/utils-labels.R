# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "ir100"              = "Incidence Rate",
  "nia"                = "NIA",
  "pia"                = "PIA",
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
  "part_ident"         = "Number of Identified Partners",
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

# Conversion between scenario name and final label
scenarios_labels <- c(
  "no_ident_no_prep"          = "Neither Partner Services or PrEP",
  "no_ident"                  = "No Partner Services",
  "base_atlanta_complete"     = "Atlanta Complete",
  "base_atlanta_missing"      = "Atlanta Missing",
  "base_atlanta_complete_alt" = "Atlanta Complete Alt",
  "ident_max"                 = "All Max",
  "ident_max_test"            = "Max ID + Test",
  "ident_max_prep"            = "Max ID + Test + PrEP",
  "ident_max_tx"              = "Max ID + Test + Tx",
  "test_100"                  = "ATL ID & Max Test",
  "prep_100"                  = "ATL ID & Max PrEP",
  "test_prep_100"             = "ATL ID & Max Test + PrEP",
  "tx_100"                    = "ATL ID & Max Tx",
  "ident_x2"                  = "ALT ID x2",
  "ident_x2_test_100"         = "ATL ID x2 & Max Test",
  "ident_x2_prep_100"         = "ATL ID x2 & Max PrEP",
  "ident_x2_test_prep_100"    = "ATL ID x2 & Max Test + PrEP",
  "ident_x2_tx_100"           = "ATL ID x2 & Max Tx"
)


# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)
fmts[["ir100"]] <- scales::label_number(0.01)
fmts[["pia"]] <- scales::label_percent(0.1)
fmts[["prep_cov"]] <- scales::label_percent(0.1)
fmts[["hiv_diag"]] <- scales::label_percent(0.1)
fmts[["hiv_tx"]] <- scales::label_percent(0.1)
fmts[["hiv_supp"]] <- scales::label_percent(0.1)
fmts[["ident_dist0"]] <- scales::label_percent(1)
fmts[["ident_dist1"]] <- scales::label_percent(0.001)
fmts[["ident_dist2"]] <- scales::label_percent(0.001)
fmts[["ident_dist3p"]] <- scales::label_percent(0.001)
fmts[["prop_found_indexes"]] <- scales::label_percent(0.1)


make_ordered_labels <- function(nms, named_labels) {
  ordered_labels <- named_labels[nms]
  ordered_labels <- paste0(seq_along(ordered_labels), "-", ordered_labels)
  names(ordered_labels) <- nms

  ordered_labels
}
