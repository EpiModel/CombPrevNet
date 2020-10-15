
##
## 04. Epidemic Model Burnin, Stage 1, Parameter Storage
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##


## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
                   tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
                   trans.scale = c(2.44, 0.424, 0.270),
                   riskh.start = 52 * 59,
                   prep.start = (52 * 60) + 1,
                   prep.start.prob = 0.66)

saveRDS(param, "data/input/param.burnin1.rds")
