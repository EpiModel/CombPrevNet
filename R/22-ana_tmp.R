source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

scenarios_files <- fs::dir_ls("out/scenarios")
scenarios <- fs::path_file(scenarios_files)

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t_test.csv"))
