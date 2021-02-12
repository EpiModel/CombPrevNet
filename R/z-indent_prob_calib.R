## The observed number of identified partners in Atlanta is 0.432 and
## the observed number of partners in my simulations is
## x = c(1.04, 1.48, 3.91) (main, casl, oof)
##
## option 1 - we enforce part.indent.main.prob == part.indent.casl.prob == 2 * part.indent.oof.prob
p_temp <- 0.432 / sum(x * c(1, 1, 1/2))
p_temp
# [1] 0.09653631
p <- c(p_temp, p_temp, p_temp / 2)
p
# [1] 0.09653631 0.09653631 0.04826816# number of partners identified by type
p * x
# [1] 0.1003978 0.1428737 0.1887285# validation
sum(p * x)
# [1] 0.432

## option 2
##
## we force each partnership type to give the same "number" of partners
p <- 0.432 / 3 / x
p
# [1] 0.13846154 0.09729730 0.03682864# number of partners identified by type
p * x
# [1] 0.144 0.144 0.144# validation
sum(p * x)
# [1] 0.432
