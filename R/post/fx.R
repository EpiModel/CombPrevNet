
## Analysis Fx

gather_netsim <- function(fn) {
  for (i in seq_along(fn)) {
    if (i == 1) {
      out <- list()
    }
    load(fn[i])
    out[[i]] <- sim
    cat("\nFile", fn[i], "complete")
  }
  class(out) <- "netsim.list"
  return(out)
}

plot_netsim_list <- function(x, var, ylim) {
  
  pal <- brewer_ramp(n = length(x), plt = "Set1", delete.lights = TRUE)
  
  if (class(x) == "netsim.list") {
    for (i in seq_along(x)) {
      if (i == 1) {
        plot(x[[i]], y = var, mean.col = pal[i], qnts = FALSE, ylim = ylim)
      } else {
        plot(x[[i]], y = var, mean.col = pal[i], qnts = FALSE, add = TRUE)
      }
    }
  } else {
    for (i in seq_along(x)) {
      load(x[i])
      if (i == 1) {
        plot(sim, y = var, mean.col = pal[i], qnts = FALSE, ylim = ylim)
      } else {
        plot(sim, y = var, mean.col = pal[i], qnts = FALSE, add = TRUE)
      }
    }
  }
  legend("topleft", legend = paste("Run", seq_along(x)), lty = 1, lwd = 2,
         cex = 0.8, bty = "n", col = pal)
}

dis_per <- function(x,y){
  num <- abs(x-y)
  avg <- num/y
  avg <- mean(avg, na.rm = TRUE)
  return(avg)
}

dis_cos <- function(x,y){
  num <- sum(x*y)
  denom <- sqrt(sum(x^2))*sqrt(sum(y^2))
  cos.sim <- num/denom
  return(cos.sim)
}

dis_euc <- function(x,y){
  dis <- x-y
  dis2 <- dis^2
  dis2 <- sum(dis2, na.rm = TRUE)
  euc <- sqrt(dis2)
  return(euc)
}

dis_log <- function(x,y){
  log.x <- log10(x)
  log.y <- log10(y)
  log.diff <- abs(log.x - log.y)
  avg <- mean(log.diff, na.rm = TRUE)
}

mod_select <- function(tdf, stats, fun){
  num <- apply()
  data <- apply(tdf[,c(2,5)], 1, dis_cos, y = stats)
  #min <- which(datas == min(data, na.rm = TRUE))
  return(min)
}

calc_quants_prev <- function(x, var, at = 520, mult = 1, round = 1, qnt.low = 0.025, qnt.high = 0.975) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(x$epi[[var]][at, ])*mult
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}

calc_quants_ir <- function(x, var, qnt.low = 0.025, qnt.high = 0.975, round = 2) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(colMeans(tail(x$epi[[var]], 52)))
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  out <- sprintf("%.2f", out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}
