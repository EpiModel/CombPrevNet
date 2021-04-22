# Convinience function to get the probability of identification
# See "R/z-indent_prob_calib.R"

ident_prob_calculator <- function(target = 0.432, pos = 1:3, method = 2) {
  x <- c(1.1, 1.6, 9.7) # observed partnerships
  if (method == 1) {
    p_temp <- target / sum(x * c(1, 1, 1/2))
    p <- c(p_temp, p_temp, p_temp / 2)
    p
  } else if (method ==2) {
    p <- target / 3 / x
  } else {
    stop("`method should be 1 or 2 (default)")
  }
}
