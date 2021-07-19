source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

scenarios <- names(c(sc_base, sc_fig2))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
readr::write_csv(d_table, paste0("out/tables/t_test.csv"))

