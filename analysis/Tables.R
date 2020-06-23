
suppressPackageStartupMessages(library("EpiModelHIV"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("foreach"))

system("scp analysis/fx.R mox:/gscratch/csde/sjenness/combprev/")
source("fx.R")

## CombPrev Table 1

load("data/sim.n1000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 1001:1018
doParallel::registerDoParallel(parallel::detectCores())
t1set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()
t1set <- do.call("rbind", t1set)
t1 <- full_join(ref, t1set)
t1 <- add_column(t1, scenario = 1000:1018, .before = 1)
t1

write_csv(t1, "data/T1.csv")


## CombPrev Table 2

load("data/sim.n2000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 2)
ref

cf.sims <- 2001:2018
doParallel::registerDoParallel(parallel::detectCores())
t2set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 2)
}
doParallel::stopImplicitCluster()
t2set <- do.call("rbind", t2set)

t2 <- full_join(ref, t2set)
t2 <- add_column(t2, scenario = 2000:2018, .before = 1)
t2

write_csv(t2, "data/T2.csv")


## CombPrev Table 3

load("data/sim.n3000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 3)
ref

cf.sims <- 3001:3017
cf.sims <- cf.sims[-c(7, 13)]
doParallel::registerDoParallel(parallel::detectCores())
t3set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 3)
}
doParallel::stopImplicitCluster()
t3set <- do.call("rbind", t3set)

t3 <- full_join(ref, t3set)
t3 <- add_column(t3, scenario = c(3000:3005, 3007:3011, 3013:3017), .before = 1)
t3

write_csv(t3, "data/T3.csv")


## CombPrev Table 4

load("data/sim.n4000.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 4)
ref

cf.sims <- 4001:4017
cf.sims <- cf.sims[-c(7, 13)]
doParallel::registerDoParallel(parallel::detectCores())
t4set <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 4)
}
doParallel::stopImplicitCluster()
t4set <- do.call("rbind", t4set)

t4 <- full_join(ref, t4set)
t4 <- add_column(t4, scenario = c(4000:4005, 4007:4011, 4013:4017), .before = 1)
t4

write_csv(t4, "data/T4.csv")


## Extra Tables Support Figure 2 (Dual Contour Plots)

# prep-linked

load("data/sim.n6500.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 6501:6503
doParallel::registerDoParallel(parallel::detectCores())
tset <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()
tset <- do.call("rbind", tset)
ta <- full_join(ref, tset)
ta <- add_column(ta, scenario = 6500:6503, .before = 1)
ta

write_csv(ta, "data/TF2a.csv")


# prep-unlinked

load("data/sim.n6504.rda")
sim.base <- sim
ref <- epi_stats(sim.base, otable = 1)
ref

cf.sims <- 6505:6507
doParallel::registerDoParallel(parallel::detectCores())
tset <- foreach(i = 1:length(cf.sims)) %dopar% {
  fn <- list.files(path = "data/",
                   pattern = as.character(cf.sims[i]), full.names = TRUE)
  load(fn)
  sim.comp <- sim
  epi_stats(sim.base, sim.comp, otable = 1)
}
doParallel::stopImplicitCluster()
tset <- do.call("rbind", tset)
ta <- full_join(ref, tset)
ta <- add_column(ta, scenario = 6504:6507, .before = 1)
ta

write_csv(ta, "data/TF2b.csv")

## Receive from Hyak

system("scp mox:/gscratch/csde/sjenness/combprev/data/*.csv analysis/")
