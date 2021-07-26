library(tidyverse)
library(RcppRoll)
library(metR)
theme_set(theme_classic())

create_var_df <- function(df, scen, var) {
  tt <- filter(df, scenario == scen)
  tt <- select(tt, c("sim", "batch", "time", var))
  tt <- pivot_wider(tt,
                    id_cols = time,
                    names_from = c(sim, batch),
                    values_from = var,
                    names_prefix = "s",
                    names_sep = ".")
  tt <- as.data.frame(tt)
  return(tt)
}

create_quants_df <- function(df, low = 0.25, high = 0.75) {
  df <- select(df, -time)
  out <- t(apply(df, 1, quantile, c(0.5, low, high), TRUE))
  return(out)
}

draw_quants <- function(x, col) {
  xx <- c(x$year, rev(x$year))
  yy <- c(x[, 2], rev(x[, 3]))
  polygon(xx, yy, col = col, border = NA)
}

apply_roll <- function(x, n) {
  for (j in 1:ncol(x)) {
    x[, j] <- RcppRoll::roll_meanr(x[, j], n = n)
    x[1:(n - 1), j] <- x[n, j]
  }
  return(x)
}

lsmooth <- function(x) {
  for (j in 1:ncol(x)) {
    x[, j] <- supsmu(x = 1:nrow(x), y = x[, j])$y
  }
  x$year <- (1:nrow(x))/52
  return(x)
}

# Figure 1 ---------------------------------------------------------------------

df <- readRDS("data/output/figure1.rds")
names(df)
table(df$scenario)
table(df$scenario)

var <- "ir100"
roll <- 104
alpha <- 0.08

pdf("out/Fig1.pdf", height = 6, width = 12)
par(mar = c(3,3,1,1), mgp = c(2,1,0))
h1 <- create_var_df(df, scen = "Atlanta Complete", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
h4 <- as.data.frame(tail(h3, 520))
h5 <- lsmooth(h4)


plot(h5$year, h5[, 1], type = "l", ylim = c(0.8, 1.4), col = 1, lwd = 1.5, lty = 1,
     xlab = "Year", ylab = "Incidence / 100 PYAR")
draw_quants(h5, col = adjustcolor(1, alpha.f = alpha))

h1 <- create_var_df(df, scen = "Max all", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
h4 <- as.data.frame(tail(h3, 520))
h5 <- lsmooth(h4)
lines(h5$year, h5[, 1], type = "l", col = 2, lwd = 1.5)
draw_quants(h5, col = adjustcolor(2, alpha.f = alpha))

h1 <- create_var_df(df, scen = "Max ID + Test + PrEP", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
h4 <- as.data.frame(tail(h3, 520))
h5 <- lsmooth(h4)
lines(h5$year, h5[, 1], type = "l", col = 3, lwd = 1.5)
draw_quants(h5, col = adjustcolor(3, alpha.f = alpha))

h1 <- create_var_df(df, scen = "Max ID + Test + tx init/reinit", var)
h2 <- apply_roll(h1, roll)
h3 <- create_quants_df(h2, low = 0.25, high = 0.75)
h4 <- as.data.frame(tail(h3, 520))
h5 <- lsmooth(h4)
lines(h5$year, h5[, 1], type = "l", col = 4, lwd = 1.5)
draw_quants(h5, col = adjustcolor(4, alpha.f = alpha))

legend("bottomleft", legend = c("Base", "Max Screen/PrEP", "Max Screen/ART", "Max All"),
       col = c(1, 3, 4, 2), lty = 1, lwd = 2.5, cex = 0.85, bty = "n")
dev.off()


# Figure 2 ---------------------------------------------------------------------

df <- readRDS("data/output/figure2.rds")
names(df)

f1a <- filter(df, grp == "Base partner services")
f1b <- filter(df, grp == "Max partner services")

loess1a <- loess(pia ~ index * partner, data = f1a, span = 0.05)
fit1a <- expand.grid(list(index = seq(min(f1a$index), max(f1a$index), length.out = 100),
                          partner = seq(min(f1a$partner), max(f1a$partner), length.out = 100)))
fit1a$pia <- as.numeric(predict(loess1a, newdata = fit1a))
head(fit1a, 25)

loess1b <- loess(pia ~ index * partner, data = f1b, span = 0.05)
fit1b <- expand.grid(list(index = seq(min(f1b$index), max(f1b$index), length.out = 100),
                          partner = seq(min(f1b$partner), max(f1b$partner), length.out = 100)))
fit1b$pia <- as.numeric(predict(loess1b, newdata = fit1b))
head(fit1b, 25)

fit1a$grp <- "Base"
fit1b$grp <- "Max"

fit1 <- rbind(fit1a, fit1b)

f1 <- ggplot(fit1, aes(index, partner)) +
  geom_raster(aes(fill = pia), interpolate = TRUE) +
  geom_contour(aes(z = pia), col = "white", alpha = 0.5, lwd = 0.5) +
  # geom_text_contour(aes(z = pia), stroke = 0.1, size = 3.5) +
  theme_minimal() +
  theme(panel.spacing = unit(1.5, "lines")) +
  facet_grid(cols = vars(grp)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Partner Identification Probability", x = "Index Initiation Probability") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "B", direction = 1)
f1

ggsave(
  paste0("out/Fig2.pdf"),
  device = "pdf",
  height = 6, width = 12,
  units = "in"
)

# fig3 test * prep * pia -------------------------------------------------------

df <- readRDS("data/output/figure3.rds")
names(df)
head(df)
table(df$grp)

f1a <- filter(df, grp == "Index prob 90%, Partner prob 25%")
f1b <- filter(df, grp == "Index prob 90%, Partner prob 50%")

loess1a <- loess(pia ~ prep * test, data = f1a, span = 0.2)
fit1a <- expand.grid(list(prep = seq(min(f1a$prep), max(f1a$prep), length.out = 100),
                          test = seq(min(f1a$test), max(f1a$test), length.out = 100)))
fit1a$pia <- as.numeric(predict(loess1a, newdata = fit1a))
head(fit1a, 25)

loess1b <- loess(pia ~ prep * test, data = f1b, span = 0.2)
fit1b <- expand.grid(list(prep = seq(min(f1b$prep), max(f1b$prep), length.out = 100),
                          test = seq(min(f1b$test), max(f1b$test), length.out = 100)))
fit1b$pia <- as.numeric(predict(loess1b, newdata = fit1b))
head(fit1b, 25)

fit1a$grp <- "Partner Prob = 25%"
fit1b$grp <- "Partner Prob = 50%"

fit1 <- rbind(fit1a, fit1b)

f1 <- ggplot(fit1, aes(test, prep)) +
  geom_raster(aes(fill = pia), interpolate = TRUE) +
  geom_contour(aes(z = pia), col = "white", alpha = 0.5, lwd = 0.5) +
  # geom_text_contour(aes(z = pia), stroke = 0.1, size = 3.5) +
  theme_minimal() +
  theme(panel.spacing = unit(1.5, "lines")) +
  facet_grid(cols = vars(grp)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Partner PrEP Initiation Probability", x = "Partner HIV Screening Probability") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "B", direction = 1)
f1

ggsave(
  paste0("out/Fig3.pdf"),
  device = "pdf",
  height = 6, width = 12,
  units = "in"
)


# fig4 test * tx (re)init * pia ------------------------------------------------

df <- readRDS("data/output/figure4.rds")
names(df)
head(df)
table(df$grp)

f1a <- filter(df, grp == "Index prob 90%, Partner prob 25%")
f1b <- filter(df, grp == "Index prob 90%, Partner prob 50%")

loess1a <- loess(pia ~ tx * test, data = f1a, span = 0.15)
fit1a <- expand.grid(list(tx = seq(min(f1a$tx), max(f1a$tx), length.out = 100),
                          test = seq(min(f1a$test), max(f1a$test), length.out = 100)))
fit1a$pia <- as.numeric(predict(loess1a, newdata = fit1a))
head(fit1a, 25)

loess1b <- loess(pia ~ tx * test, data = f1b, span = 0.15)
fit1b <- expand.grid(list(tx = seq(min(f1b$tx), max(f1b$tx), length.out = 100),
                          test = seq(min(f1b$test), max(f1b$test), length.out = 100)))
fit1b$pia <- as.numeric(predict(loess1b, newdata = fit1b))
head(fit1b, 25)

fit1a$grp <- "Partner Prob = 25%"
fit1b$grp <- "Partner Prob = 50%"

fit1 <- rbind(fit1a, fit1b)

f1 <- ggplot(fit1, aes(test, tx)) +
  geom_raster(aes(fill = pia), interpolate = TRUE) +
  geom_contour(aes(z = pia), col = "white", alpha = 0.5, lwd = 0.5) +
  # geom_text_contour(aes(z = pia), stroke = 0.1, size = 3.5) +
  theme_minimal() +
  theme(panel.spacing = unit(1.5, "lines")) +
  facet_grid(cols = vars(grp)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = "Partner ART Engagement Probability", x = "Partner HIV Screening Probability") +
  scale_fill_viridis(discrete = FALSE, alpha = 1, option = "B", direction = 1)
f1

ggsave(
  paste0("out/Fig4.pdf"),
  device = "pdf",
  height = 6, width = 12,
  units = "in"
)