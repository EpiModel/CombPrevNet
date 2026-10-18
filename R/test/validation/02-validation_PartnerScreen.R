## Process Schematic 2: Testing of Identified of Partners of Incident HIV+ MSM
## --------------------------------------------------------------- ##

suppressMessages(library("EpiModelHIV"))

netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est <- readRDS("data/input/netest.rds")

param <- param_msm(epistats = epistats,
                   netstats = netstats,
                   part.hiv.test.rate  = c(1, 1, 1),
                   part.ident.main.window =  52,
                   part.ident.casl.window  = 52,
                   part.ident.ooff.window  = 52,
                   part.ident.main.prob = 1,
                   part.ident.casl.prob = 1,
                   part.ident.ooff.prob = 1,
                   part.index.window = Inf,
                   part.ident.start = 1
)

init <- init_msm()

control1 <- control_msm(simno = 1001,
                        nsteps = 100,
                        ncores = 1,
                        nsims = 1)

sim <- list()

set.seed(123)

burnin <- netsim(est, param, init, control1)



control2 <- control_msm(simno = 1001,
                        start = 101,
                        nsteps = 200,
                        ncores = 1,
                        nsims = 5,
                        initialize.FUN = reinit_msm)


sim[[1]] <- netsim(burnin, param, init, control2)


## hiv.scrn.rate: HIV testing rate for partners of incident HIV+ MSM
# Low screen rate
param$part.hiv.test.rate <- c(0.5, 0.25, 0.125)

sim[[2]] <- netsim(burnin, param, init, control2)

# No screening for identified partners
param$hiv.scrn.rate <- c(0, 0, 0)

sim[[3]] <- netsim(burnin, param, init, control2)

# High screen rate
param$part.hiv.test.rate <- c(0.95, 0.75, 0.5)

sim[[4]] <- netsim(burnin, param, init, control2)

plot(sim[[1]], y = "part.screened", xlim = c(100, 200),
     xlab = "Timestep", ylab = "Partners Screened",
     main = "Validation: part.hiv.test.rate", ylim = c(0, 150))
plot(sim[[2]], y = "part.screened",
     mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim[[3]], y = "part.screened",
     mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim[[4]], y = "part.screened",
     mean.col = "yellow", qnts.col = "yellow", add = TRUE)
legend(100, 150, legend = c("part.hiv.test.rate = 1, 1, 1",
                            "part.hiv.test.rate = 0, 0, 0",
                            "part.hiv.test.rate = 0.5, 0.25, 0.125",
                            "part.hiv.test.rate = 0.95, 0.75, 0.5"),
       text.col = c("blue", "green", "red", "yellow"),
       cex = 0.75, bg = "lightblue")

saveRDS(sim, file = "process2.sim.rds")
