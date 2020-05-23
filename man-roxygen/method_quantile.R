#' @param ... `(numeric())` \cr
#' Points to evaluate the quantile (inverse-cdf) function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param lower.tail `logical(1)` \cr
#' If `TRUE` (default), probabilities are `X ≤ x`, otherwise, `X > x`.
#'@param log.p `logical(1)` \cr
#'If `TRUE` returns quantile evaluated at exp(`p`). Default is `FALSE`.
#'@param simplify `logical(1)` \cr
#'If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#'[data.table::data.table][data.table].
#'@param data [array] \cr
#'Alternative method to specify points to evaluate. If univariate then rows correspond with number
#'of points to evaluate and columns correspond with number of variables to evaluate. In the special
#'case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#'to the distribution in the vector to evaluate.
#'
#'@examples
#'b <- Binomial$new()
#'b$quantile(0.42)
#'b$quantile(log(0.42), log.p = TRUE, lower.tail = TRUE)
#'b$quantile(data = matrix(c(0.1,0.2)))
