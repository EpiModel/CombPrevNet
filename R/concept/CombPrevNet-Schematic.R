
library("ergm")

rm(list=ls())
n <- 250

net <- network.initialize(n, directed = FALSE)
fit <- ergm(net ~ edges, target.stats = 2*n/2)
sim <- simulate(fit)

par(mar = c(0,0,0,0))
b <- plot(sim)

v.bord <- rep("grey80", 1)
v.col <- rep(4, n)

# directory for images
setwd("~/Box/CombPrevNet/presentation")

## Full network
pdf("Step1-AllNodes.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

degs <- EpiModel::get_degree(sim)
focal.node <- which.max(degs)

# Highlight infected
prev <- 0.25
all.inf <- union(focal.node, 1:round(n*prev))
# v.col[all.inf] <- adjustcolor(2, alpha.f = 0.6)
v.col[all.inf] <- 2

pdf("Step2-HighlightInf.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# De-Highlight Suppressed
prev.vsupp <- 0.4
ids.vsupp <- sample(all.inf, round(length(all.inf)*prev.vsupp))

pdf("Step3-DehighlightVSupp.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
v.col[ids.vsupp] <- adjustcolor(2, alpha.f = 0.4)
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Plot PrEP Users
non.iso <- which(degs > 1)
non.iso.neg <- setdiff(non.iso, all.inf)

prep.cov <- 0.25
ids.prep <- sample(non.iso.neg, round(length(non.iso.neg)*prep.cov))

pdf("Step4-PlotPrEP.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
v.col[ids.prep] <- 3
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Remove any concordant negative edges
inf.edges <- do.call("c", sapply(all.inf, function(x) get.edgeIDs(sim, x)))
non.inf.edges <- setdiff(1:nrow(as.edgelist(sim)), inf.edges)
sim2 <- sim
sim2 <- delete.edges(sim2, non.inf.edges)

pdf("Step5-RemoveConcNegEdges.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# De-Highlight isolates
ids.iso.neg <- setdiff(which(EpiModel::get_degree(sim2) == 0), all.inf)

pdf("Step6-DehighlightIso.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
v.col[ids.iso.neg] <- adjustcolor(4, alpha.f = 0.05)
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Remove Vsupp edges
vsupp.edges <- do.call("c", sapply(ids.vsupp, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, vsupp.edges)

pdf("Step7-RemoveVsuppEdges.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Remove PrEP edges
prep.edges <- do.call("c", sapply(ids.prep, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, prep.edges)

pdf("Step8-RemovePrEPEdges.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Add PrEP to negative connected
ids.neg <- setdiff(1:n, all.inf)
ids.nprep <- setdiff(ids.neg, ids.prep)                       
ids.nprep.deg1 <- intersect(ids.nprep, which(EpiModel::get_degree(sim2)>0))                          

v.col[ids.nprep.deg1] <- 3

pdf("Step9-AddPrEP.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

## improve Vsupp
ids.nvsupp.deg1 <- intersect(setdiff(all.inf, ids.vsupp), which(EpiModel::get_degree(sim2)>0))
ids.vsupp2 <- union(ids.vsupp, ids.nvsupp.deg1)
v.col[ids.vsupp2] <- adjustcolor(2, alpha.f = 0.4)

pdf("Step10-AddVsupp.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# Remove new Vsupp edges
vsupp2.edges <- do.call("c", sapply(ids.vsupp2, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, vsupp2.edges)

pdf("Step11-RemoveLastEdges.pdf", height = 8.5, width = 8.5)
par(mar = c(0,0,0,0))
plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
dev.off()

# dynamic network

# Network model
library("EpiModel")
library("ndtv")

nw <- network_initialize(n = 250, directed = FALSE)
# neg nprep, neg prep, pos nsupp, pos supp
hiv <- sample(1:4, 250, TRUE, prob = c(0.50, 0.25, 0.10, 0.15))
nw <- set_vertex_attribute(nw, "hiv", hiv)
nw

dvcol <- c(4, 3, 2, adjustcolor(2, alpha.f = 0.4))
dvcol.all <- dvcol[hiv]

formation <- ~edges
target.stats <- c(125)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 20)
est <- netest(nw, formation, target.stats, coef.diss)

# Epidemic model
param <- param.net(inf.prob = 0)
init <- init.net(i.num = 10)
control <- control.net(type = "SI", nsteps = 30, nsims = 1)
sim <- netsim(est, param, init, control)

# Network movie
nw <- get_network(sim)
# nw <- color_tea(nw)
slice.par <- list(start = 1, end = 30, interval = 1,
                  aggregate.dur = 1, rule = "any")
render.par <- list(tween.frames = 10, show.time = FALSE)
plot.par <- list(mar = c(0, 0, 0, 0))
compute.animation(nw, slice.par = slice.par, verbose = TRUE)

# Final movie!
saveGIF(
render.animation(
        nw,
        vertex.cex = 1.25,
        render.par = render.par,
        plot.par = plot.par,
        vertex.col = dvcol.all,
        vertex.border = adjustcolor("black", alpha.f = 0.25),
        edge.col = "darkgrey",
        displaylabels = FALSE),
ani.height = 700,
ani.width = 700,
ani.res = 250
)


