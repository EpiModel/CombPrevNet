
##
## 06. Epidemic Model Burnin, Stage 1, Simulation Selection
## CombPrevNet (https://github.com/EpiModel/CombPrevNet)
##

suppressMessages(library("EpiModelHIV"))
suppressMessages(library("dplyr"))
suppressMessages(library("foreach"))
source("post/fx.R")

list.files("data/output/")
fn <- list.files("data/output/", full.names = TRUE, pattern = "1002")


tdf <- data.frame("batch" = 0, "cc.dx.B" = 0, "cc.dx.H" = 0, "cc.dx.W" = 0,
                  "i.prev.dx.B" = 0, "i.prev.dx.H" = 0, "i.prev.dx.W" = 0,
                  "cc.linked1m.B" = 0, "cc.linked1m.H" = 0, "cc.linked1.W" = 0,
                  "cc.vsupp.B" = 0, "cc.vsupp.H" = 0, "cc.vsupp.W" = 0)

# doParallel::registerDoParallel(parallel::detectCores())
# tdf <- foreach(i = 1:5) %dopar% {
#   load(fn[i])
#   f <- function(j) {
#     df <- as.data.frame(x = sim, sim = j)
#     df <- select(df, cc.dx, i.prev.dx, cc.linked1m, cc.vsupp)
#     df <- tail(df, 52)
#     batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3],
#                    collapse = "."), j, sep = ".")
#     out <- c(batch, colMeans(df))
#     return(out)
#   }
#   t(sapply(1:sim$control$nsims, f))
# }
# colnames(tdf) <- c("batch", "cc.dx.B", "cc.dx.H", "cc.dx.W",
#                    "i.prev.dx.B", "i.prev.dx.H", "i.prev.dx.W",
#                    "cc.linked1m.B", "cc.linked1m.H", "cc.linked1.W",
#                    "cc.vsupp.B", "cc.vsupp.H", "cc.vsupp.W")
#
# tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
# tdf[2:13] <- sapply(tdf[2:13], as.numeric)


for (i in seq_len(fn)) {
  load(fn[i])
  for (j in 1:25) {
    f <- function(j) {
      df <- as.data.frame(x = sim, sim = j)
      df <- select(df, cc.dx.B, cc.dx.H, cc.dx.W,
                   i.prev.dx.B, i.prev.dx.H, i.prev.dx.W,
                   cc.linked1m.B, cc.linked1m.H, cc.linked1m.W,
                   cc.vsupp.B, cc.vsupp.H, cc.vsupp.W)
      df <- tail(df, 52)
      batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3],
                           collapse = "."), j, sep = ".")
      out <- c(batch, colMeans(df))
      return(out)
    }
    tdf <- rbind(tdf, f(j))
  }
}

tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
tdf[2:13] <- sapply(tdf[2:13], as.numeric)

## Model Performance
stats <- c(0.804, 0.799, 0.88, 0.33, 0.126, 0.085,
           0.62, 0.65, 0.76, 0.55, 0.6, 0.72)

data <- apply(tdf[, c(2:13)], 1, dis_per, y = stats)
temp.per <- which(data >= quantile(data, 0.999))
tdf[temp.per, ]

data <- apply(tdf[, c(2:13)], 1, dis_euc, y = stats)
temp.euc <- which(data <= quantile(data, 0.01))
tdf[temp.euc, ]

data <- apply(tdf[, c(2:13)], 1, dis_log, y = stats)
temp.log <- which(data <= quantile(data, 0.01))
tdf[temp.log, ]

data <- apply(tdf[, c(2:13)], 1, dis_cos, y = stats)
temp.cos <- which(data >= quantile(data, 0.999, na.rm = TRUE))
tdf[temp.cos, ]

tdf.select <- list("Percent" = tdf[temp.per, ],
                   "Euclidean" = tdf[temp.euc, ],
                   "Log-Diff" = tdf[temp.log, ],
                   "Cosine Sim." = tdf[temp.cos, ])

load("data/output/sim.n1002.36.rda")
ls()
s11 <- get_sims(sim, sims = 24)

df <- as.data.frame(s11)
df <- select(df, cc.dx.B, cc.dx.H, cc.dx.W,
             i.prev.dx.B, i.prev.dx.H, i.prev.dx.W,
             cc.linked1m.B, cc.linked1m.H, cc.linked1m.W,
             cc.vsupp.B, cc.vsupp.H, cc.vsupp.W)
df <- tail(df, 1)
df
colMeans(df)

# Save as best-fitting
sim <- s11

saveRDS(sim, file = "data/input/burnin1.rds", compress = "xz")
saveRDS(tdf, file = "data/input/burnin1-tdf.rds", compress = "xz")
saveRDS(tdf.select, file = "data/input/burnin1-tdfsel.rds", compress = "xz")
