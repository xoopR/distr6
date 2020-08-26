#' @include lines_discrete.R lines_continuous.R
#'
#' @title Superimpose Distribution Functions Plots for a distr6 Object
#' @author Chengyang Gao, Runlong Yu and Shuhan Liu
#'
#' @description One of six plots can be selected to be superimposed in the plotting window,
#' including: pdf, cdf, quantile, survival, hazard and cumulative hazard.
#'
#' @param x \code{distr6} object.
#' @param fun vector of functions to plot, one or more of: "pdf","cdf","quantile", "survival",
#' "hazard", and "cumhazard"; partial matching available.
#' @param npoints number of evaluation points.
#' @param ... graphical parameters.
#'
#' @details Unlike the \code{\link{plot.Distribution}} function, no internal checks are performed
#' to ensure that the added plot makes sense in the context of the current plotting window.
#' Therefore this function assumes that the current plot is of the same value support, see examples.
#'
#' @seealso \code{\link{plot.Distribution}} for plotting a \code{distr6} object.
#'
#' @examples
#' plot(Normal$new(mean = 2), "pdf")
#' lines(Normal$new(mean = 3), "pdf", col = "red", lwd = 2)
#'
#' \dontrun{
#' # The code below gives examples of how not to use this function.
#' # Different value supports
#' plot(Binomial$new(), "cdf")
#' lines(Normal$new(), "cdf")
#'
#' # Different functions
#' plot(Binomial$new(), "pdf")
#' lines(Binomial$new(), "cdf")
#'
#' # Too many functions
#' plot(Binomial$new(), c("pdf", "cdf"))
#' lines(Binomial$new(), "cdf")
#' }
#'
#' @export
lines.Distribution <- function(x, fun, npoints = 3000, ...) {

  #######################################################################
  #######                         validations                     #######
  #######################################################################

  if (!testUnivariate(x) | testMixture(x)) {
    stop("Currently only plotting univariate, discrete or continuous, distributions are supported.")
  }

  plotFuns <- c("pdf", "cdf", "quantile", "survival", "hazard", "cumhazard")
  # check user input plot names are correct
  if (length(fun) > 1) {
    message("Only the first function is used, the rest are ignored.")
    fun <- fun[[1]]
  }
  fun <- unique(plotFuns[charmatch(fun, plotFuns)])

  if (any(is.na(fun))) {
    stop("Function unrecognised, should be one of: ", paste0(plotFuns, collapse = ","))
  }

  if ("cdf" %in% fun & is.null(x$.__enclos_env__$private$.cdf)) {
    message("This distribution does not have a cdf expression. Use the
            FunctionImputation decorator to impute a numerical cdf.")
    fun <- fun[!(fun %in% c("cdf", "survival", "hazard", "cumhazard"))]
  }

  if ("pdf" %in% fun & is.null(x$.__enclos_env__$private$.pdf)) {
    message("This distribution does not have a pdf expression. Use the
            FunctionImputation decorator to impute a numerical pdf.")
    fun <- fun[!(fun %in% c("pdf", "hazard"))]
  }

  if ("quantile" %in% fun & is.null(x$.__enclos_env__$private$.quantile)) {
    message("This distribution does not have a quantile expression. Use the
            FunctionImputation decorator to impute a numerical quantile.")
    fun <- fun[!(fun %in% c("quantile"))]
  }

  if (length(fun) == 0) {
    stop("No plottable functions.")
  }

  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################


  if (testDiscrete(x) & x$properties$support$properties$countability == "countably finite") {
    plotStructure <- data.table::data.table(points = unlist(x$properties$support$elements))
    plotStructure$cdf <- x$cdf(plotStructure$points)
  } else {
    plotStructure <- data.table::data.table(cdf = seq(0, 1, length.out = npoints))
    plotStructure$points <- x$quantile(plotStructure$cdf)
    plotStructure <- plotStructure[, 2:1]

    if (testDiscrete(x)) {
      plotStructure <- stats::aggregate(cdf ~ points, plotStructure, max)
    }
  }

  plotStructure$pdf <- x$pdf(plotStructure$points)

  if ("survival" %in% fun) {
    plotStructure$survival <- 1 - plotStructure$cdf
  }
  if ("hazard" %in% fun) {
    plotStructure$hazard <- plotStructure$pdf / (1 - plotStructure$cdf)
  }
  if ("cumhazard" %in% fun) {
    plotStructure$cumhazard <- -log(1 - plotStructure$cdf)
  }

  if (testContinuous(x)) {
    .lines_continuous(fun, plotStructure, ...)
  }
  else if (testDiscrete(x)) {
    .lines_discrete(fun, plotStructure, ...)
  }

  invisible(data.table::data.table(plotStructure))
}
