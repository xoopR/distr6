#' @title Plotting Distribution Functions for a VectorDistribution
#'
#' @description Helper function to more easily plot distributions inside a [VectorDistribution].
#'
#' @param x [VectorDistribution].
#' @param fun function to plot, one of: "pdf","cdf","quantile", "survival", "hazard", "cumhazard".
#' @param topn `integer`. First n distributions in the [VectorDistribution] to plot.
#' @param ind `integer`. Indices of the distributions in the [VectorDistribution] to plot. If given
#' then `topn` is ignored.
#' @param cols `character`. Vector of colours for plotting the curves. If missing `1:9` are used.
#' @param ... Other parameters passed to [plot.Distribution].
#'
#' @details
#' If `topn` and `ind` are both missing then all distributions are plotted if there are 10 or less
#' in the vector, otherwise the function will error.
#'
#' @seealso [plot.Distribution]
#'
#' @examples
#' \dontrun{
#' # Plot pdf of Normal distribution
#' vd <- VectorDistribution$new(list(Normal$new(), Normal$new(mean = 2)))
#' plot(vd)
#' plot(vd, fun = "surv")
#' plot(vd, fun = "quantile", ylim = c(-4, 4), col = c("blue", "purple"))
#' }
#' @export
plot.VectorDistribution <- function(x, fun = "pdf", topn, ind, cols, ...) {
  if (getR6Class(x) == "MixtureDistribution" | getR6Class(x) == "ProductDistribution") {
    stopf("Plotting of `%s`s not currently supported.", getR6Class(x))
  }

  if (!missing(ind)) {
    dist <- x[ind]
  } else if (!missing(topn)) {
    dist <- x[1:topn]
  } else {
    if (nrow(x$modelTable) > 10) {
      stop("More than 10 distributions in vector, please supply 'topn' or 'ind'.")
    } else {
      dist <- x
    }
  }
  if (getR6Class(dist) != "VectorDistribution") {
    col <- if (missing(cols)) 1 else cols[1]
    graphics::plot(dist, fun = fun, col = col, ...)
  } else {
    col <- if (missing(cols)) 1 else cols[1]
    graphics::plot(dist[1], fun = fun, col = col, ...)
    for (i in 2:nrow(dist$modelTable)) {
      col <- if (missing(cols)) i else cols[i]
      graphics::lines(dist[i], fun, col = col, ...)
    }
  }

}
