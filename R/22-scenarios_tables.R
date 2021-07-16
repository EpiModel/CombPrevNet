lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

# T2
scenarios <- names(c(sc_base, sc_t2))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t2.csv"))

# T3a
scenarios <- names(c(sc_base, sc_t3a))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t3a.csv"))

# T3b
scenarios <- names(c(sc_base, sc_t3b))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t3b.csv"))
