
## build master.sh script ##

library("EpiModelHPC")
setwd("intervention/")

# Table 1 -----------------------------------------------------------------

vars <- list(HTRB = 0.00385,
             HTRH = 0.00380,
             HTRW = 0.00690,
             LNT = TRUE,
             TIPB = 0.1775,
             TIPH = 0.190,
             TIPW = 0.2521,
             THPB = 0.0062,
             THPH = 0.0055,
             THPW = 0.0031)

# no targeting
mults <- c(1, 2, 5, 10)
cfvars <- vars
cfvars$HTRB <- c(cfvars$HTRB * mults, 1/52, 1/26, 1/13)
cfvars$HTRH <- c(cfvars$HTRH * mults, 1/52, 1/26, 1/13)
cfvars$HTRW <- c(cfvars$HTRW * mults, 1/52, 1/26, 1/13)

sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T1.sh",
              runsim.file = "runsim.sh",
              build.runsim = TRUE,
              simno.start = 1000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted BMSM
mults <- c(2, 5, 10)
cfvars <- vars
cfvars$HTRB <- c(cfvars$HTRB * mults, 1/52, 1/26, 1/13)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T1.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted HMSM
mults <- c(2, 5, 10)
cfvars <- vars
cfvars$HTRH <- c(cfvars$HTRH * mults, 1/52, 1/26, 1/13)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T1.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Table 2 -----------------------------------------------------------------

vars2 <- vars
vars2$LNT <- FALSE

# no targeting
cfvars <- vars2
mults <- c(1, 2, 5, 10)
cfvars$HTRB <- c(cfvars$HTRB * mults, 1/52, 1/26, 1/13)
cfvars$HTRH <- c(cfvars$HTRH * mults, 1/52, 1/26, 1/13)
cfvars$HTRW <- c(cfvars$HTRW * mults, 1/52, 1/26, 1/13)

sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T2.sh",
              simno.start = 2000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# targeted BMSM
cfvars <- vars2
mults <- c(2, 5, 10)
cfvars$HTRB <- c(cfvars$HTRB * mults, 1/52, 1/26, 1/13)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T2.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted HMSM
cfvars <- vars2
mults <- c(2, 5, 10)
cfvars$HTRH <- c(cfvars$HTRH * mults, 1/52, 1/26, 1/13)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T2.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")



# Table 3 -----------------------------------------------------------------

# no targeting
mults <- c(1, 1.50, 2)
cfvars <- vars
cfvars$TIPB <- c(cfvars$TIPB * mults, 1/4, 1/2, 1)
cfvars$TIPH <- c(cfvars$TIPH * mults, 1/4, 1/2, 1)
cfvars$TIPW <- c(cfvars$TIPW * mults, 1/4, 1/2, 1)

sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T3.sh",
              simno.start = 3000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# targeted BMSM
cfvars <- vars
mults <- c(1.5, 2)
cfvars$TIPB <- c(cfvars$TIPB * mults, 1/4, 1/2, 1)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T3.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted HMSM
cfvars <- vars
mults <- c(1.5, 2)
cfvars$TIPH <- c(cfvars$TIPH * mults, 1/4, 1/2, 1)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T3.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Table 4 -----------------------------------------------------------------

# no targeting
mults <- round(c(1, 1/1.5, 1/2, 1/5, 1/10, 1/20), 6)
cfvars <- vars
cfvars$THPB <- c(cfvars$THPB * mults)
cfvars$THPH <- c(cfvars$THPH * mults)
cfvars$THPW <- c(cfvars$THPW * mults)

sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T4.sh",
              simno.start = 4000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# targeted BMSM
cfvars <- vars
mults <- round(c(1/1.5, 1/2, 1/5, 1/10, 1/20), 6)
cfvars$THPB <- c(cfvars$THPB * mults)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T4.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted HMSM
cfvars <- vars
mults <- round(c(1/1.5, 1/2, 1/5, 1/10, 1/20), 6)
cfvars$THPH <- c(cfvars$THPH * mults)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.T4.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Figure 1 ----------------------------------------------------------------

# no targeting
mults <- seq(1, 10, 0.1)
cfvars <- vars
cfvars$HTRB <- c(cfvars$HTRB * mults)
cfvars$HTRH <- c(cfvars$HTRH * mults)
cfvars$HTRW <- c(cfvars$HTRW * mults)

sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.F1.sh",
              simno.start = 5000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted BMSM
cfvars <- vars
cfvars$HTRB <- c(cfvars$HTRB * mults)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.F1.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

# targeted HMSM
cfvars <- vars
cfvars$HTRH <- c(cfvars$HTRH * mults)
sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.F1.sh",
              append = TRUE,
              ckpt = TRUE,
              nsims = 500,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Figure 2 ----------------------------------------------------------------

vars <- list(MULT1 = seq(1, 10, 1),
             MULT2 = seq(1, 10, 1),
             LNT = c(TRUE, FALSE))

sbatch_master(vars = vars,
              expand.vars = TRUE,
              master.file = "master.F2.sh",
              runsim.file = "runsim.F2.sh",
              build.runsim = TRUE,
              rscript.file = "sim.F2.R",
              simno.start = 6000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 112,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Extra analyses requested by CDC clearance - January 2020
vars <- list(MULT1 = c(1, 2, 5, 10, 1, 2, 5, 10),
             MULT2 = c(1, 2, 5, 10, 1, 2, 5, 10),
             LNT = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

sbatch_master(vars = vars,
              expand.vars = FALSE,
              master.file = "master.F2b.sh",
              runsim.file = "runsim.F2.sh",
              build.runsim = FALSE,
              rscript.file = "sim.F2.R",
              simno.start = 6500,
              append = FALSE,
              ckpt = FALSE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")



# KD CROI Abstract --------------------------------------------------------

vars <- list(HTRB = 0.00385,
             HTRH = 0.00380,
             HTRW = 0.00690,
             LNT = TRUE,
             TIPB = 0.1775,
             TIPH = 0.190,
             TIPW = 0.2521,
             THPB = 0.0062,
             THPH = 0.0055,
             THPW = 0.0031)

mults <- c(1, 2)
cfvars <- vars
cfvars$HTRB <- c(cfvars$HTRB * mults, 1/52, 1/13)
cfvars$HTRH <- c(cfvars$HTRH * mults, 1/52, 1/13)
cfvars$HTRW <- c(cfvars$HTRW * mults, 1/52, 1/13)


sbatch_master(vars = cfvars,
              expand.vars = FALSE,
              master.file = "master.KD.sh",
              runsim.file = "runsim.sh",
              build.runsim = TRUE,
              simno.start = 7000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

vars <- list(MULT1 = 10,
             MULT2 = 10,
             LNT = TRUE)

sbatch_master(vars = vars,
              expand.vars = TRUE,
              master.file = "master.KD.sh",
              runsim.file = "runsim.F2.sh",
              build.runsim = TRUE,
              rscript.file = "sim.F2.R",
              simno.start = 7100,
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

vars <- list(MULT1 = 1,
             MULT2 = 10,
             LNT = TRUE)

sbatch_master(vars = vars,
              expand.vars = TRUE,
              master.file = "master.KD.sh",
              runsim.file = "runsim.F2.sh",
              rscript.file = "sim.F2.R",
              simno.start = 7200,
              append = TRUE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")


# Figure 3 ----------------------------------------------------------------

vars <- list(MULT1 = c(1, 2, 5, 10),
             MULT2 = c(1, 2, 5, 10),
             LNT = rep(c(TRUE, FALSE), each = 4))

sbatch_master(vars = vars,
              expand.vars = FALSE,
              master.file = "master.F3.sh",
              runsim.file = "runsim.F3.sh",
              build.runsim = TRUE,
              rscript.file = "sim.F3.R",
              simno.start = 8000,
              append = FALSE,
              ckpt = TRUE,
              nsims = 1000,
              ncores = 28,
              walltime = "00:30:00",
              mem = "100G")

