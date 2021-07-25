#' @title Coerce matrix to vector of WeightedDiscrete
#' @description Coerces matrices to a [VectorDistribution] containing
#' [WeightedDiscrete] distributions. Number of distributions are the number
#' of rows in the matrix, number of `x` points are number of columns in the
#' matrix.
#' @param obj [matrix]. Column names correspond to `x` in [WeightedDiscrete],
#' so this method only works if all distributions (rows in the matrix) have the
#' same points to be evaluated on. Elements correspond to either the pdf
#' or cdf of the distribution (see below).
#' @param fun Either `"pdf"` or `"cdf"`, passed to [WeightedDiscrete] and tells
#' the constructor if the elements in `obj` correspond to the pdf or cdf of
#' the distribution.
#' @param decorators Passed to [VectorDistribution].
#' @return A [VectorDistribution]
#' @export
#' @examples
#' pdf <- runif(200)
#' mat <- matrix(pdf, 20, 10)
#' mat <- t(apply(mat, 1, function(x) x / sum(x)))
#' colnames(mat) <- 1:10
#' as.Distribution(mat, fun = "pdf")
as.Distribution <- function(obj, fun, decorators = NULL) {
  UseMethod("as.Distribution")
}

#' @rdname as.Distribution
#' @export
as.Distribution.matrix <- function(obj, fun, decorators = NULL) {

  if (is.null(colnames(obj))) {
    stop("'obj' must have column names")
  }

  if (fun %nin% c("pdf", "cdf")) {
    stop("'fun' should be one of 'pdf', 'cdf'")
  }

  x <- as.numeric(colnames(obj))
  obj <- apply(obj, 1, function(.x) {
    out <- list(fun = .x, x = x)
    names(out)[[1]] <- fun
    out
  })

  VectorDistribution$new(
    distribution = "WeightedDiscrete",
    params = obj,
    decorators = decorators
  )

}
