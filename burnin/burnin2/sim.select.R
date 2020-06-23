
suppressMessages(library("EpiModelHIV"))
suppressMessages(library("tidyverse"))
suppressMessages(library("foreach"))

list.files("data/")
fn <- list.files("data/", pattern = "sim", full.names = TRUE)
fn

doParallel::registerDoParallel(parallel::detectCores())
tdf <- foreach(i = 1:length(fn)) %dopar% {
  load(fn[i])
  f <- function(j) {
    df <- as.data.frame(x = sim, sim = j)
    df <- select(df, prepCurr, prepElig)
    df <- tail(df, 52)
    pFrac <- mean(df$prepCurr/df$prepElig)
    batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
    out <- c(batch, pFrac)
    return(out)
  }
  t(sapply(1:sim$control$nsims, f))
}

tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
names(tdf) <- c("batch", "pFrac")
tdf[2] <- sapply(tdf[2], as.numeric)
head(tdf, 20); str(tdf)
save(tdf, file = "data/hold/burnin2.tdf-all.rda")

targets <- c(0.15)
tdf$diff <- abs(tdf$pFrac - targets[1])

head(plyr::arrange(tdf, diff), 25)

load("data/sim.n400.2.rda")
ls()
s1 <- get_sims(sim, sims = 3)

df <- as.data.frame(s1)
df <- select(df, prepCurr, prepElig)
df <- tail(df, 52)
pFrac <- mean(df$prepCurr/df$prepElig)
pFrac


# Save as best-fitting
sim <- s1

saveRDS(sim, file = "est/burnin2.ATL.Prep15.rda", compress = "xz")

load("data/sim.n400.2.rda")
save(sim, file = "data/hold/sim.n400.2.rda", compress = "xz")
