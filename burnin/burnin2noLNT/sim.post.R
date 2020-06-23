
## Calibrate Starting PrEP Level

suppressWarnings(library("EpiModelHIV"))

load("burnin/burnin2noLNT/data/sim.n500.rda")

sim$param$prep.start.prob
sim$param$prep.require.lnt
sim$param$prep.discont.rate

# df <- as.data.frame(sim)
# names(df)
# plot(sim, y = "prepCurr")

sim <- mutate_epi(sim, pFrac = prepCurr / prepElig)
sim <- mutate_epi(sim, pFracA = prepCurr / num)

par(mar = c(3,3,1,1), mgp = c(2,1,0))
plot(sim, y = "pFrac", ylim = c(0, 0.25))
abline(v = 52*5, h = 0.15, lty = 2)

stat <- as.numeric(sim$epi$pFrac[52*5, ])
summary(stat)
