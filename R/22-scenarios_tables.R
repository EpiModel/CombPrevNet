source("R/utils-create_outcomes.R")

scenarios <- fs::dir_ls("out/scenarios")
base <- scenarios[grepl("base_atlanta", scenarios)]

# # Maxs
# max_sc <- c(
#   scenarios[grepl("max", scenarios)],
#   scenarios[grepl("no_ident", scenarios)]
# )

# df <- make_outcomes(base, c(base, max_sc))
# readr::write_csv( df, paste0("out/tables/max.csv"))

# T2
t2_sc <- scenarios[grepl("t2_", scenarios)]

df <- make_outcomes(base, c(base, t2_sc))
readr::write_csv( df, paste0("out/tables/t2.csv"))

# T2b
t2b_sc <- scenarios[grepl("t2b_", scenarios)]

df <- make_outcomes(base, c(base, t2b_sc))
readr::write_csv( df, paste0("out/tables/t2b.csv"))

# T2c
t2c_sc <- scenarios[grepl("t2c_", scenarios)]

df <- make_outcomes(base, c(base, t2c_sc))
readr::write_csv( df, paste0("out/tables/t2c.csv"))

# T3
t3_sc <- scenarios[grepl("t3_", scenarios)]

df <- make_outcomes(base, c(base, t3_sc))
readr::write_csv( df, paste0("out/tables/t3.csv"))
