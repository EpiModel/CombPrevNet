
library("ergm")

n <- 250

net <- network.initialize(n, directed = FALSE)
fit <- ergm(net ~ edges, target.stats = 2*n/2)
sim <- simulate(fit)

par(mar = c(0,0,0,0))
b <- plot(sim)

v.bord <- rep("grey80", 1)
v.col <- rep(4, n)

plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

degs <- EpiModel::get_degree(sim)
focal.node <- which.max(degs)

prev <- 0.2
all.inf <- union(focal.node, 1:round(n*prev))
# v.col[all.inf] <- adjustcolor(2, alpha.f = 0.6)
v.col[all.inf] <- 2

plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

prev.vsupp <- 0.5
ids.vsupp <- sample(all.inf, round(length(all.inf)*prev.vsupp))

v.col[ids.vsupp] <- adjustcolor(2, alpha.f = 0.4)
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

non.iso <- which(degs > 1)
non.iso.neg <- setdiff(non.iso, all.inf)

prep.cov <- 0.25
ids.prep <- sample(non.iso.neg, round(length(non.iso.neg)*prep.cov))

v.col[ids.prep] <- 3
plot(sim, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

inf.edges <- do.call("c", sapply(all.inf, function(x) get.edgeIDs(sim, x)))
non.inf.edges <- setdiff(1:nrow(as.edgelist(sim)), inf.edges)
sim2 <- sim
sim2 <- delete.edges(sim2, non.inf.edges)

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

ids.iso.neg <- setdiff(which(EpiModel::get_degree(sim2) == 0), all.inf)
sim2 <- delete.vertices(sim2, ids.iso.neg)

remaining.ids <- setdiff(1:n, ids.iso.neg)
v.col <- v.col[remaining.ids]
b <- b[remaining.ids, ]
ids.prep <- ids.prep[remaining.ids]
ids.vsupp <- ids.vsupp[remaining.ids]
all.inf <- all.inf[remaining.ids]

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

vsupp.edges <- do.call("c", sapply(ids.vsupp, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, vsupp.edges)

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

prep.edges <- do.call("c", sapply(ids.prep, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, prep.edges)

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

ids.neg.deg1 <- intersect(setdiff(1:n, all.inf), which(EpiModel::get_degree(sim2)>0))
ids.prep2 <- union(ids.prep, ids.neg.deg1)
v.col[ids.prep2] <- 3

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

## improve vsupp

ids.nvsupp.deg1 <- intersect(setdiff(all.inf, ids.vsupp), which(EpiModel::get_degree(sim2)>0))
ids.vsupp2 <- union(ids.vsupp, ids.nvsupp.deg1)
v.col[ids.vsupp2] <- adjustcolor(2, alpha.f = 0.4)

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)

vsupp2.edges <- do.call("c", sapply(ids.vsupp2, function(x) get.edgeIDs(sim2, x)))
sim2 <- delete.edges(sim2, vsupp2.edges)

plot(sim2, coord = b,
     edge.col = "grey60",
     vertex.border = v.bord,
     vertex.cex = 1.75,
     vertex.col = v.col)
