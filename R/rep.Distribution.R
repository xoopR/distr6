#' @title Replicate Distribution into Vector, Mixture, or Product
#' @description Replicates a constructed distribution into either a
#'
#' * [VectorDistribution] (`class = "vector"`)
#' * [ProductDistribution] (`class = "product"`)
#' * [MixtureDistribution] (`class = "mixture"`)
#'
#' If the distribution is not a custom [Distribution] then uses the more efficient
#'  `distribution/params` constructor, otherwise uses `distlist`.
#'
#' @param x [Distribution]
#' @param times `(integer(1))` Number of times to replicate the distribution
#' @param class `(character(1))` What type of vector to create, see description.
#' @param ... Additional arguments, currently unused.
#'
#' @examples
#' rep(Binomial$new(), 10)
#' rep(Gamma$new(), 2, class = "product")
#'
#' @export
rep.Distribution <- function(x, times, class = c("vector", "product", "mixture"), ...) {

  assertDistribution(x)
  checkmate::assertIntegerish(times, lower = 1)
  class <- match.arg(class)

  if (getR6Class(x) == "Distribution") {
    get(paste0(toproper(class), "Distribution"))$new(distlist = rep(list(x), times))
  } else {
    get(paste0(toproper(class), "Distribution"))$new(distribution = getR6Class(x),
                                                     params = rep(list(x$parameters()$values()),
                                                                  times))
  }
}
