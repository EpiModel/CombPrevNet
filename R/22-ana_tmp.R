source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

scenarios_files <- fs::dir_ls("out/scenarios")
scenarios <- fs::path_file(scenarios_files)

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
readr::write_csv(d_raw, paste0("out/tables/t_test.csv"))


