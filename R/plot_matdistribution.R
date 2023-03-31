#' @title Plotting Distribution Functions for a Matrix Distribution
#'
#' @description Helper function to more easily plot a [Matdist].
#'
#' @param x [Matdist].
#' @param fun function to plot, one of: "pdf","cdf", "survival", "hazard", "cumhazard".
#' @param ... Other parameters passed to [matplot].
#' @details Essentially just a wrapper around [matplot].
#' @seealso [plot.Distribution] [plot.VectorDistribution]
#' @examples
#' \dontrun{
#' pdf <- runif(200)
#' mat <- matrix(pdf, 20, 10)
#' mat <- t(apply(mat, 1, function(x) x / sum(x)))
#' colnames(mat) <- 1:10
#' d <- as.Distribution(mat, fun = "pdf")
#' plot(d, "pdf", xlab = "x", ylab = "p(x)")
#' plot(d, "cdf", xlab = "x", ylab = "F(x)")
#' plot(d, "survival", xlab = "x", ylab = "S(x)")
#' plot(d, "hazard", xlab = "x", ylab = "h(x)")
#' plot(d, "cumhazard", xlab = "x", ylab = "H(x)")
#' }
#' @export
plot.Matdist <- function(x,
  fun = c("pdf", "cdf", "survival", "hazard", "cumhazard"), ...) {

  fun <- match.arg(fun)

  if (fun %in% c("pdf", "cdf")) {
    val <- gprm(x, fun)
  } else if (fun == "survival") {
    val <- 1 - gprm(x, "cdf")
  } else if (fun == "hazard") {
    "*" %=% gprm(x, c("cdf", "pdf"))
    val <- pdf / (1 - cdf)
  } else if (fun == "cumhazard") {
    val <- -log(1 - gprm(x, "cdf"))
  }

  times <- as.numeric(colnames(val))
  graphics::matplot(t(val), type = "l", xaxt = "n", ...)
  where <- round(seq(1, length(times), length.out = 5))
  axis(1, where, times[where])

}
