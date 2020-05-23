#' @param n `(numeric(1))` \cr
#' Number of points to simulate from the distribution.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#'
#' @examples
#' b <- Binomial$new()
#' b$rand(10)
#'
#' mvn <- MultivariateNormal$new()
#' mvn$rand(5)
