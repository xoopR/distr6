#' @include plot_discrete.R plot_continuous.R
#'
#' @title Plot Distribution Functions for a distr6 Object
#' @author Chengyang Gao, Runlong Yu and Shuhan Liu
#'
#' @description Six plots, which can be selected with \code{fun} are available for discrete and
#' continuous univariate distributions: pdf, cdf, quantile, survival, hazard and cumulative
#' hazard. By default, the first two are plotted side by side.
#'
#' @param x \code{distr6} object.
#' @param fun vector of functions to plot, one or more of: "pdf","cdf","quantile", "survival",
#'  "hazard", "cumhazard", and "all"; partial matching available.
#' @param npoints number of evaluation points.
#' @param plot logical; if TRUE (default), figures are displayed in the plot window; otherwise a
#' [data.table::data.table()] of points and calculated values is returned.
#' @param ask logical; if TRUE, the user is asked before each plot, see [graphics::par()].
#' @param arrange logical; if TRUE (default), margins are automatically adjusted with
#' [graphics::layout()] to accommodate all plotted functions.
#' @param ... graphical parameters, see details.
#'
#'
#' @details
#' The evaluation points are calculated using inverse transform on a uniform grid between 0 and 1
#' with length given by \code{npoints}. Therefore any distribution without an analytical `quantile`
#' method will first need to be imputed with the [FunctionImputation] decorator.
#'
#' The order that the functions are supplied to \code{fun} determines the order in which they are
#' plotted, however this is ignored if \code{ask} is \code{TRUE}. If \code{ask} is \code{TRUE} then
#' \code{arrange} is ignored. For maximum flexibility in plotting layouts, set \code{arrange} and
#' \code{ask} to \code{FALSE}.
#'
#' The graphical parameters passed to \code{...} can either apply to all plots or selected plots.
#' If parameters in \code{\link[graphics]{par}} are prefixed with the plotted function name, then
#' the parameter only applies to that function, otherwise it applies to them all. See examples for
#' a clearer description.
#'
#' @seealso [lines.Distribution]
#'
#' @examples
#' \dontrun{
#' # Plot pdf and cdf of Normal
#' plot(Normal$new())
#'
#' # Colour both plots red
#' plot(Normal$new(), col = "red")
#'
#' # Change the colours of individual plotted functions
#' plot(Normal$new(), pdf_col = "red", cdf_col = "green")
#'
#' # Interactive plotting in order - par still works here
#' plot(Geometric$new(),
#'   fun = "all", ask = TRUE, pdf_col = "black",
#'   cdf_col = "red", quantile_col = "blue", survival_col = "purple",
#'   hazard_col = "brown", cumhazard_col = "yellow"
#' )
#'
#' # Return plotting structure
#' x <- plot(Gamma$new(), plot = FALSE)
#' }
#' @export
plot.Distribution <- function(x, fun = c("pdf", "cdf"), npoints = 3000,
                              plot = TRUE, ask = FALSE, arrange = TRUE, ...) {


  #######################################################################
  #######                         validations                     #######
  #######################################################################

  if (x$.__enclos_env__$private$.variates > 2) {
    stop("Distributions with over two variables not currently supported.")
  }

  plotFuns <- c("pdf", "cdf", "quantile", "survival", "hazard", "cumhazard", "all")
  fun <- unique(plotFuns[charmatch(fun, plotFuns)])
  if ("all" %in% fun) {
    fun <- plotFuns[-7]
  }

  if (any(is.na(fun))) {
    stop("Function unrecognised, should be one of: ", paste0(plotFuns, collapse = ","))
  }

  if (any(c("cdf", "survival", "hazard", "cumhazard") %in% fun) & !isCdf(x)) {
    message("This distribution does not have a cdf expression.
Use the FunctionImputation decorator to impute a numerical cdf.")
    fun <- fun[!(fun %in% c("cdf", "survival", "hazard", "cumhazard"))]
  }

  if (any(c("pdf", "hazard") %in% fun) & !isPdf(x)) {
    message("This distribution does not have a pdf expression.
Use the FunctionImputation decorator to impute a numerical pdf.")
    fun <- fun[!(fun %in% c("pdf", "hazard"))]
  }

  if (("quantile" %in% fun) & !isQuantile(x)) {
    message("This distribution does not have a quantile expression.
Use the FunctionImputation decorator to impute a numerical quantile.")
    fun <- fun[!(fun %in% c("quantile"))]
  }

  if (length(fun) == 0) {
    stop("No plottable functions.")
  }

  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################

  if (testMultivariate(x)) {
    return(.plot_multivariate(x, fun, npoints))
  } else {
    if (testDiscrete(x) & x$properties$support$properties$countability == "countably finite") {
      plotStructure <- data.table::data.table(points = unlist(x$properties$support$elements))
    } else {
      if (isQuantile(x)) {
        plotStructure <- data.table::data.table(cdf = seq(0, 1, length.out = npoints))
        plotStructure$points <- x$quantile(plotStructure$cdf)
        plotStructure <- plotStructure[, 2:1]
      } else if (isRand(x)) {
        plotStructure <- data.table::data.table(points = sort(x$rand(npoints)))
      } else {
        message("No quantile or rand available, representation may not be accurate.
Use the FunctionImputation decorator for better accuracy.")
        max <- ifelse(x$dmax == Inf, 100, x$dmax)
        min <- ifelse(x$dmin == -Inf, -100, x$dmin)
        plotStructure <- data.table::data.table(points = seq.int(min, max, length.out = npoints))
      }

      if (testDiscrete(x) & "cdf" %in% fun) {
        plotStructure <- stats::aggregate(cdf ~ points, plotStructure, max)
      }
    }

    if (any(c("cdf", "survival", "hazard", "cumhazard", "quantile") %in% fun) &
      !("cdf" %in% colnames(plotStructure))) {
      plotStructure$cdf <- x$cdf(plotStructure$points)
    }
    if (any(c("pdf", "hazard") %in% fun)) {
      plotStructure$pdf <- x$pdf(plotStructure$points)
    }

    if ("survival" %in% fun) {
      plotStructure$survival <- 1 - plotStructure$cdf
    }
    if ("hazard" %in% fun) {
      plotStructure$hazard <- plotStructure$pdf / (1 - plotStructure$cdf)
    }
    if ("cumhazard" %in% fun) {
      plotStructure$cumhazard <- -log(1 - plotStructure$cdf)
    }


    #######################################################################
    #######                     graphical parameters                #######
    #######################################################################

    if (length(fun) == 1) {
      ask <- arrange <- FALSE
    }

    if (plot) {
      if (ask | arrange) {
        def.par <- graphics::par(no.readonly = TRUE)
        graphics::par(ask = ask)
      }

      if (arrange & !ask) {
        data <- seq_len(length(fun))
        if (length(fun) == 3 | length(fun) == 5) data <- c(data, 0)

        n <- switch(length(fun),
          "1" = list(nrow = 1, ncol = 1),
          "2" = list(nrow = 1, ncol = 2),
          "3" = list(nrow = 2, ncol = 2),
          "4" = list(nrow = 2, ncol = 2),
          "5" = list(nrow = 2, ncol = 3),
          "6" = list(nrow = 2, ncol = 3)
        )

        graphics::layout(do.call(matrix, c(list(byrow = TRUE, data = data), n)))

      }

      if (testContinuous(x)) {
        .plot_continuous(fun, plotStructure, x$strprint(), ...)
      } else if (testDiscrete(x)) {
        .plot_discrete(fun, plotStructure, x$strprint(), ...)
      }

      if (ask | arrange) {
        graphics::par(def.par)
      }
    }
  }

  invisible(data.table::data.table(plotStructure))
}
