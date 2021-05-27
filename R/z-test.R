source("R/utils-create_outcomes.R")

scenarios <- fs::dir_ls("out/scenarios")

base <- scenarios[grepl("base_atlanta", scenarios)]
scenarios <- scenarios[!grepl("t3_", scenarios)]

df <- make_outcomes(base, scenarios)
readr::write_csv( df, paste0("out/tables/t2.csv"))

base <- scenarios[grepl("base_atlanta", scenarios)]
scenarios <- scenarios[!grepl("t2_", scenarios)]

df <- make_outcomes(base, scenarios)
readr::write_csv( df, paste0("out/tables/t3.csv"))
