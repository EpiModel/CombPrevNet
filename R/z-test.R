library(EpiModelHIV)
library(tidyverse)

lnt <- TRUE
source("R/utils-params.R", local = TRUE)
# orig <- readRDS("out/est/restart.rds")


control <- control_msm(
  nsteps = 20 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
  nsims = 1,
  ncores = 1
)

sim <- netsim(orig, param, init, control)

df <- as.data.frame(sim)

ggplot(df, aes(x = time)) +
  geom_line(aes(y = i.prev.dx.H)) +
  geom_hline(yintercept = 0.127)
