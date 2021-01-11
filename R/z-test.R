library(data.table)
library(ggplot2)

sim <- readRDS("out/remote_jobs/CPN_dry_run/out/sim1.rds")

dff <- as.data.table(sim)

source("R/utils-targets.R")
names(dff)[names(dff) %in% names(targets)]


