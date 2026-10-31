spades_sa_cpp <- function(X, specs, group, gw, correct, refacc, initial, L, pilot, log_every, checkpoint, savefun, regression, utility_scale, verbose) {
 .Call(`_SPADES_IND_spades_sa_cpp`, X, specs, group, gw, correct, refacc, initial, L, pilot, log_every, checkpoint, savefun, regression, utility_scale, verbose)
}
