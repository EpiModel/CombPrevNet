require(EpiModelHIV)
require(ARTnet)
require(parallel)

epistats <- build_epistats(geog.lvl = "city", geog.cat = "Atlanta", race = TRUE,
                           init.hiv.prev = rep(0.17, 3))
netparams <- build_netparams(epistats = epistats, smooth.main.dur = TRUE)
netstats <- build_netstats(epistats, netparams, expect.mort = 0.000478213,
                           network.size = 25000)


# 0. Initialize Network ---------------------------------------------------

num <- netstats$demog$num
nw <- network::network.initialize(num, directed = FALSE)

attr.names <- names(netstats$attr)
attr.values <- netstats$attr
nw <- network::set.vertex.attribute(nw, attr.names, attr.values)
nw_main <- nw_casl <- nw_inst <- nw


# 1. Main Model -----------------------------------------------------------

# Formula
model_main <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -1) +
  nodematch("race", diff = FALSE) +
  nodefactor("race", levels = -1) +
  nodefactor("deg.casl", levels = -1) +
  concurrent +
  degrange(from = 3) +
  nodematch("role.class", diff = TRUE, levels = 1:2)

# Target Stats
netstats_main <- c(
  edges = netstats$main$edges,
  nodematch_age.grp = netstats$main$nodematch_age.grp,
  nodefactor_age.grp = netstats$main$nodefactor_age.grp[-1],
  nodematch_race = netstats$main$nodematch_race_diffF,
  nodefactor_race = netstats$main$nodefactor_race[-1],
  nodefactor_deg.casl = netstats$main$nodefactor_deg.casl[-1],
  concurrent = netstats$main$concurrent,
  degrange = 0,
  nodematch_role.class = c(0, 0)
)
cbind(netstats_main)
netstats_main <- unname(netstats_main)

# Fit model
fit_main <- netest(nw_main,
                   formation = model_main,
                   target.stats = netstats_main,
                   coef.diss = netstats$main$diss.byage,
                   set.control.ergm = control.ergm(MCMLE.maxit = 500,
                                                   SAN.maxit = 3,
                                                   SAN.nsteps.times = 3),
                   verbose = FALSE)


# model_form <- model_main
# model_diss <- ~edges + nodematch("age.grp", diff = TRUE)
#
# coef_form <- fit_main$coef.form
# coef_diss <- fit_main$coef.diss$coef.crude
#
# cl <- makeCluster(15)
# clusterExport(cl, "nw")
# clusterExport(cl, "coef_form")
# clusterExport(cl, "coef_diss")
# clusterExport(cl, "model_form")
# clusterExport(cl, "model_diss")
# st <- Sys.time()
# rvp <- clusterEvalQ(cl, {
#   require(tergm)
#   simulate(nw, formation = model_form, dissolution = model_diss,
#            coef.form = coef_form, coef.diss = coef_diss, output = "stats",
#            stats.form = TRUE, time.slices=40000, time.burnin=40000,
#            control=control.simulate.network(MCMC.burnin.min=4e4, MCMC.init.maxchanges=1e8))
# })
# et <- Sys.time()
# stopCluster(cl)
# print(et - st)
#
# rvp_old <- do.call(rbind, rvp)
#
# (colMeans(rvp_old) - netstats_main)/netstats_main


model_main_dx <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = TRUE) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = TRUE) +
  nodefactor("deg.casl", levels = TRUE) +
  degrange(from = 3) +
  concurrent +
  nodematch("role.class", diff = TRUE) +
  degree(0:3)
dx_main <- netdx(fit_main, nsims = 20, ncores = 20, nsteps = 1000,
                 nwstats.formula = model_main_dx, skip.dissolution = TRUE,
                 set.control.ergm = control.simulate.ergm(MCMC.burnin = 1e5),
                 set.control.stergm = control.simulate.network(MCMC.burnin.min=1000,
                                                               MCMC.init.maxchanges=1e8))

print(dx_main)
plot(dx_main)

## old EDA:

## with 1e4 time.slices, 1e4 time.burnin, and 1e4 MCMC.burnin.min:

## Time difference of 1.117587 hours
## >
## > rvp <- do.call(rbind, rvp)
## >
## > (colMeans(rvp) - netstats_main)/netstats_main
##                  edges    nodematch.age.grp.1    nodematch.age.grp.2    nodematch.age.grp.3
##           0.0004134958           0.0057437207          -0.0090346003          -0.0007144302
##    nodematch.age.grp.4    nodematch.age.grp.5   nodefactor.age.grp.2   nodefactor.age.grp.3
##           0.0220906944          -0.0159544875          -0.0050763002           0.0002480747
##   nodefactor.age.grp.4   nodefactor.age.grp.5         nodematch.race      nodefactor.race.2
##           0.0100079818          -0.0042508551           0.0006288529           0.0271192029
##      nodefactor.race.3  nodefactor.deg.casl.1  nodefactor.deg.casl.2  nodefactor.deg.casl.3
##           0.0019526509          -0.0028106429           0.0120873576          -0.0214946376
##             concurrent                  deg3+ nodematch.role.class.0 nodematch.role.class.1
##          -0.0010229406                    NaN                    NaN                    NaN

## with 4e4 time.slices, 4e4 time.burnin, and 4e4 MCMC.burnin.min:

## Time difference of 16.22389 hours
## >
## > rvp_old <- do.call(rbind, rvp)
## >
## > (colMeans(rvp_old) - netstats_main)/netstats_main
##                  edges    nodematch.age.grp.1    nodematch.age.grp.2    nodematch.age.grp.3
##          -0.0013666453          -0.0042818212          -0.0085590665          -0.0031254697
##    nodematch.age.grp.4    nodematch.age.grp.5   nodefactor.age.grp.2   nodefactor.age.grp.3
##          -0.0022114869          -0.0043541781          -0.0056638508          -0.0025582688
##   nodefactor.age.grp.4   nodefactor.age.grp.5         nodematch.race      nodefactor.race.2
##           0.0031473760           0.0022076719          -0.0009851086          -0.0017339116
##      nodefactor.race.3  nodefactor.deg.casl.1  nodefactor.deg.casl.2  nodefactor.deg.casl.3
##          -0.0005508914           0.0012571602           0.0018582599          -0.0064827132
##             concurrent                  deg3+ nodematch.role.class.0 nodematch.role.class.1
##           0.0008697846                    NaN                    NaN                    NaN

## new EDA:

D <- netstats$main$diss.byage$duration

form_adj <- log(D)
form_adj[-1] <- form_adj[-1] - log(D[1])

coef_diss_new <- coef_diss

coef_form_new <- coef_form
coef_form_new[1:6] <- coef_form_new[1:6] + coef_diss_new - form_adj


cl <- makeCluster(15)
clusterExport(cl, "nw")
clusterExport(cl, "coef_form_new")
clusterExport(cl, "coef_diss_new")
clusterExport(cl, "model_form")
clusterExport(cl, "model_diss")
st <- Sys.time()
rvp <- clusterEvalQ(cl, {
  require(tergm)
  simulate(nw, formation = model_form, dissolution = model_diss, coef.form = coef_form_new, coef.diss = coef_diss_new, output = "stats", stats.form = TRUE, time.slices=40000, time.burnin=40000, control=control.simulate.network(MCMC.burnin.min=4e4, MCMC.init.maxchanges=1e8))
})
et <- Sys.time()
stopCluster(cl)
print(et - st)

rvp_new <- do.call(rbind, rvp)

(colMeans(rvp_new) - netstats_main)/netstats_main

## with 1e4 time.slices, 1e4 time.burnin, and 1e4 MCMC.burnin.min:

## > (colMeans(rvp_new) - netstats_main)/netstats_main
##                  edges    nodematch.age.grp.1    nodematch.age.grp.2    nodematch.age.grp.3
##          -1.383143e-03          -1.867344e-03          -1.238096e-02           4.881253e-04
##    nodematch.age.grp.4    nodematch.age.grp.5   nodefactor.age.grp.2   nodefactor.age.grp.3
##           2.905775e-02          -2.370291e-02          -7.288676e-03          -1.080103e-05
##   nodefactor.age.grp.4   nodefactor.age.grp.5         nodematch.race      nodefactor.race.2
##           1.178536e-02          -6.892597e-03          -2.469221e-03           2.467579e-02
##      nodefactor.race.3  nodefactor.deg.casl.1  nodefactor.deg.casl.2  nodefactor.deg.casl.3
##           5.217983e-04          -4.624217e-03           6.422508e-03          -1.714568e-02
##             concurrent                  deg3+ nodematch.role.class.0 nodematch.role.class.1
##          -7.336926e-04                    NaN                    NaN                    NaN
##

## with 4e4 time.slices, 4e4 time.burnin, and 4e4 MCMC.burnin.min:

## Time difference of 15.97178 hours
## >
## > rvp_new <- do.call(rbind, rvp)
## >
## > (colMeans(rvp_new) - netstats_main)/netstats_main
##                  edges    nodematch.age.grp.1    nodematch.age.grp.2    nodematch.age.grp.3
##          -0.0033384826          -0.0110679674          -0.0096837253           0.0001821379
##    nodematch.age.grp.4    nodematch.age.grp.5   nodefactor.age.grp.2   nodefactor.age.grp.3
##          -0.0026194311          -0.0063571175          -0.0068982714          -0.0023469271
##   nodefactor.age.grp.4   nodefactor.age.grp.5         nodematch.race      nodefactor.race.2
##           0.0015971012          -0.0005273369          -0.0029406792          -0.0046993903
##      nodefactor.race.3  nodefactor.deg.casl.1  nodefactor.deg.casl.2  nodefactor.deg.casl.3
##          -0.0020116909          -0.0014609466          -0.0007787280          -0.0163900988
##             concurrent                  deg3+ nodematch.role.class.0 nodematch.role.class.1
##          -0.0055795695                    NaN                    NaN                    NaN
##
