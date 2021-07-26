source("R/utils-params.R", local = TRUE)
source("R/utils-scenarios.R")
source("R/utils-create_outcomes.R")

if (!fs::dir_exists("out/tables"))
  fs::dir_create("out/tables")

if (!fs::dir_exists("out/tables_data"))
  fs::dir_create("out/tables_data")

# T2
scenarios <- names(c(sc_base, sc_t2))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t2.rds")
readr::write_csv(d_table, "out/tables/t2.csv")

# T3a
scenarios <- names(c(sc_base, sc_t3a))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t3a.rds")
readr::write_csv(d_table, "out/tables/t3a.csv")

# T3b
scenarios <- names(c(sc_base, sc_t3b))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t3b.rds")
readr::write_csv(d_table, "out/tables/t3b.csv")

# T4
scenarios <- names(c(sc_base, sc_t4[names(sc_t4) != "t4_no_ident"]))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t4.rds")
readr::write_csv(d_table, "out/tables/t4.csv")

# T5a
scenarios <- c("t5a_base", names(sc_t5a))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t5a.rds")
readr::write_csv(d_table, "out/tables/t5a.csv")

# T5b
scenarios <- c("t4_ident_max_all", names(sc_t5b))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t5b.rds")
readr::write_csv(d_table, "out/tables/t5b.csv")

# T6 - base files
scenarios <- names(c(sc_base, sc_atlanta_missing, sc_t4["t4_no_ident"]))
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

d_raw <- make_outcomes(scenarios_files[1], scenarios_files, scenarios)
d_table <- make_table(d_raw)
saveRDS(d_raw, "out/tables_data/t6.rds")
readr::write_csv(d_table, "out/tables/t6.csv")
