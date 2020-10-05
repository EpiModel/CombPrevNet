
## File processing -------------------------------------------------------------

suppressMessages(library("EpiModelHIV"))
suppressMessages(library("tidyverse"))
suppressMessages(library("foreach"))
source("post/fx.R")

list.files("data/")
fn <- list.files("data/", full.names = TRUE, pattern = "1002")


tdf <- data.frame(batch = NA, cc.dx = NA, i.prev.dx = NA, cc.linked1m = NA, cc.vsupp = NA)

for (i in 1:length(fn)) {
  load(fn[i])
  for (j in 1:25){
    f <- function(j) {
      df <- as.data.frame(x = sim, sim = j)
      df <- select(df, cc.dx, i.prev.dx, cc.linked1m, cc.vsupp)
      df <- tail(df, 52)
      batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
      out <- c(batch, colMeans(df))
      return(out)
    }
    tdf <- rbind(tdf, f(j))
  }
}

tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
names(tdf)[1:5] <- c("batch", "cc.dx", "i.prev.dx", "cc.linked1m","cc.vsupp")
tdf[2:5] <- sapply(tdf[2:5], as.numeric)

## Model Performance
stats <- c(0.84,0.17,0.68,0.63)

data <- apply(tdf[,c(2:5)], 1, dis_per, y = stats)
temp.per <- which(data >= quantile(data, 0.999))
tdf[temp.per,]

data <- apply(tdf[,c(2:5)], 1, dis_euc, y = stats)
temp.euc <- which(data <= quantile(data, 0.01))
tdf[temp.euc,]

data <- apply(tdf[,c(2:5)], 1, dis_log, y = stats)
temp.log <- which(data <= quantile(data, 0.01))
tdf[temp.log,]

data <- apply(tdf[,c(2:5)], 1, dis_cos, y = stats)
temp.cos <- which(data >= quantile(data, 0.999))
tdf[temp.cos,]

tdf.select <- list("Percent" = tdf[temp.per,], "Euclidean" = tdf[temp.euc,],
                   "Log-Diff" = tdf[temp.log,], "Cosine Sim." = tdf[temp.cos,])

load("data/sim.n1001.43.rda")
ls()
s11 <- get_sims(sim, sims = 10)

df <- as.data.frame(s11)
df <- select(df, cc.dx, i.prev.dx, cc.linked1m, cc.vsupp)
df <- tail(df,1)
df
colMeans(df)

# Save as best-fitting
sim <- s11

saveRDS(sim, file = "est/burnin.ATL.1race.rds", compress = "xz")
saveRDS(tdf, file = "est/tdf.ATL.1race.rds", compress = "xz")
saveRDS(tdf.select, file = "est/tds.sel.ATL.1race.rds", compress = "xz")
