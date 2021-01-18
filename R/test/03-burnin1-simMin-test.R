
#
## 03. Epidemic Model Burnin, Stage 1, Parameter Calibration
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

## Packages
library("methods")
pkgload::load_all("../EpiModelHIV-p")

## Environmental Arguments
pull_env_vars()

nsims <- ncores <- 1

## Parameters
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")
est      <- readRDS("out/est/netest.rds")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
                   tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
                   trans.scale = c(2.44, 0.424, 0.270),
                   riskh.start = 1,
                   prep.start = 26,
                   prep.start.prob = 0.66,

                   truncate.plist = 1,
                   part.ident.start = Inf,
                   part.index.window = 0,
                   part.ident.main.window = 120,
                   part.ident.casl.window = 120,
                   part.ident.ooff.window = 12,
                   part.ident.main.prob = 0.5,
                   part.ident.casl.prob = 0.5,
                   part.ident.ooff.prob = 0.5,
                   part.hiv.test.rate = c(0.5, 0.5, 0.5),
                   part.prep.start.prob = 0.5,
                   part.tx.init.prob = c(0.6, 0.6, 0.8),
                   part.tx.halt.prob = c(0.00001, 0.00001, 0.00001),
                   part.tx.reinit.prob = c(0.05, 0.05, 0.05)
)
init <- init_msm()

# pkgload::load_all("~/git/EpiModelHIV-p")
control <- control_msm(
  simno = fsimno,
  nsteps = 52 * 10,
  nsims = ncores,
  ncores = ncores,
  save.nwstats = TRUE,
  # raw.output = TRUE,
  verbose = FALSE
)

## Simulation
sim <- netsim(est, param, init, control)




library(dplyr)
sim[[1]]$temp$plist %>%
  as_tibble() %>%
  filter(!is.na(stop)) %>%
  print(n = 500)

saveRDS(sim, file = "tsim.rds")

system("scp mox:/gscratch/csde/sjenness/CombPrevNet/tsim.rds data/output/")
sim <- readRDS("data/output/tsim.rds")

nws1 <- get_nwstats(sim, network = 1)
nws2 <- get_nwstats(sim, network = 2)
nws3 <- get_nwstats(sim, network = 3)

library(ggplot2)
library(dplyr)

ggplot(nws1, aes(time, mdeg, col = as.factor(sim))) +
  geom_line(alpha = 0.5) +
  ylim(0, 1) +
  theme_minimal()

mmd <- group_by(nws1, time) %>%
  summarise(mmd = mean(mdeg))
plot(mmd, type = "l", ylim = c(0.3, 0.6))

ggplot(nws2, aes(time, mdeg, col = as.factor(sim))) +
  geom_line(alpha = 0.5) +
  ylim(0, 1) +
  theme_minimal()

ggplot(nws3, aes(time, mdeg, col = as.factor(sim))) +
  geom_line(alpha = 0.5) +
  ylim(0, 0.2) +
  theme_minimal()

par(mar = c(3, 3, 1, 1))
plot(sim, y = "num", ylim = c(0, 20000))
plot(sim, y = "dep.gen", mean.smooth = FALSE, ylim = c(0, 5))
plot(sim, y = "dep.AIDS")

df <- tail(as.data.frame(sim, out = "mean"), 1000)
head(df)
totD <- df$dep.gen + df$dep.AIDS
mean(totD/df$num, na.rm = TRUE)

df_sim <- as.data.frame(sim)

ggplot(df_sim, aes(x = time, y = i.prev, col = as.factor(sim))) +
  geom_line()



