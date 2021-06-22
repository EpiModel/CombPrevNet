lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

# T2
scenarios <- names(c(sc_base, sc_t2))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t2.csv"))

# T2b
scenarios <- names(c(sc_base, sc_t2b))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t2b.csv"))

# T2c
scenarios <- names(c(sc_base, sc_t2c))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t2c.csv"))

# T3
scenarios <- names(c(sc_base, sc_t3))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

df <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
readr::write_csv(df, paste0("out/tables/t3.csv"))
