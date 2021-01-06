library(data.table)
library(ggplot2)

sim <- readRDS("out/remote_jobs/CPN_dry_run/out/sim1.rds")

dff <- as.data.table(sim)

plot(dff[sim == 1, i.prev.B])
plot(dff[sim == 1, cc.vsupp])

print(names(dff), max = 200)
dff[sim == 1, incid.B]
dff[sim == 1, incid]

sum(sim[[1]]$attr$diag.status, na.rm = T) / sum(sim[[1]]$attr$status, na.rm = T)
sim[[1]]$epi$i.prev.dx[520] / sim[[1]]$epi$i.prev[520]
sim[[1]]$epi$i.num[520]

sum(sim[[1]]$attr$status, na.rm = T) / sum(sim[[1]]$attr$active, na.rm = T)
sim[[1]]$epi$i.prev[520]

sim[[1]]$epi$cc.prev.dx
print(names(sim[[1]]$epi), max = 200)
plot(sim[[1]]$epi$cc.dx)
sim[[1]]$epi$i.prev.dx / sim[[1]]$epi$i.prev
sim[[1]]$epi$cc.dx

epi <- EpiModelHIV::prevalence_msm(sim[[1]], 521)$epi
attr <- sim[[1]]$attr

epi$i.prev[521]
epi$cc.dx.B[521]

sum(attr$diag.status == 3, na.rm = TRUE) /
  sum(attr$status == 1 , na.rm = TRUE)
