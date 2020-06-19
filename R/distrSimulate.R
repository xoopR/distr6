#' @title Simulate from a Distribution
#' @description Helper function to quickly simulate from a distribution with given parameters.
#' @param n number of points to simulate.
#' @param distribution distribution to simulate from, corresponds to `ClassName` of `distr6`
#' distribution, abbreviations allowed.
#' @param pars parameters to pass to `distribution`. If omitted then `distribution` defaults used.
#' @param simplify if `TRUE` (default) only the simulations are returned, otherwise the constructed
#' distribution is also returned.
#' @param seed passed to [set.seed]
#' @param ... additional optional arguments for [set.seed]
#' @return If `simplify` then vector of `n` simulations, otherwise list of simulations and
#' distribution.
#' @seealso [rand]
#' @export
distrSimulate <- function(n = 100, distribution = "Normal", pars = list(), simplify = TRUE,
                          seed, ...) {
  if (!missing(seed)) {
    set.seed(seed, ...)
  }
  dist <- match.arg(distribution, listDistributions(T))
  # checkmate::assert(distribution %in% listDistributions(T))
  dist <- do.call(get(dist)$new, pars)

  if (length(n) > 1) {
    n <- length(n)
  }

  if (n < 0) {
    n <- 0
  }

  sim <- dist$rand(n)

  if (simplify) {
    return(sim)
  } else {
    return(list(Distribution = dist, Simulations = sim))
  }
}
