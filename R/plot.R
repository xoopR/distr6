#' @include plot_discrete.R plot_continuous.R
#'
#' @title Plot Distribution Functions for a distr6 Object
#' @author Chengyang Gao, Runlong Yu and Shuhan Liu
#'
#' @description Six plots, which can be selected with \code{fun} are available for discrete and
#' continuous univarite distributions: pdf, cdf, quantile, survival, hazard and cumulative
#' hazard. By default, the first two are plotted side by side.
#'
#' @param x \code{distr6} object.
#' @param fun vector of functions to plot, one or more of: "pdf","cdf","quantile", "survival", "hazard", "cumhazard", and "all"; partial matching available.
#' @param npoints number of evaluation points.
#' @param plot logical; if TRUE (default), figures are displayed in the plot window; otherwise a \code{data.table} of points and calculated values is returned.
#' @param ask logical; if TRUE, the user is asked before each plot, see \code{\link[graphics]{par}}.
#' @param arrange logical; if TRUE (default), margins are automatically adjusted with \code{\link[graphics]{layout}} to accomodate all plotted functions.
#' @param ... graphical parameters, see details.
#'
#'
#' @details
#' The evaluation points are calculted using inverse transform on a uniform grid between 0 and 1 with
#' length given by \code{npoints}. Therefore any distribution without an analytical \code{quantile} method
#' will first need to be imputed with the \code{\link{FunctionImputation}} decorator.
#'
#' The order that the functions are supplied to \code{fun} determines the order in which they are
#' plotted, however this is ignored if \code{ask} is \code{TRUE}. If \code{ask} is \code{TRUE} then
#' \code{arrange} is ignored. For maximum flexibility in plotting layouts, set \code{arrange} and
#' \code{ask} to \code{FALSE}.
#'
#' The graphical parameters passed to \code{...} can either apply to all plots or selected plots. If
#' parameters in \code{\link[graphics]{par}} are prefixed with the plotted function name, then the
#' parameter only applies to that funciton, otherwise it applies to them all. See examples for a clearer
#' description.
#'
#' @seealso \code{\link{lines.Distribution}} for superimposing a distr6 object and \code{\link{listDistributions}}
#' for plottable distributions.
#'
#' @examples
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
#' plot(Geometric$new(), fun = "all", ask = TRUE, pdf_col = "black",
#'   cdf_col = "red", quantile_col = "blue", survival_col = "purple",
#'   hazard_col = "brown", cumhazard_col = "yellow")
#'
#' # Return plotting structure
#' x = plot(Gamma$new(), plot = FALSE)
#'
#' @export
plot.Distribution <- function(x, fun=c('pdf','cdf'), npoints = 3000,
                              plot = TRUE, ask = FALSE, arrange = TRUE,...){


  #######################################################################
  #######                         validations                     #######
  #######################################################################

  if(!testUnivariate(x) | testMixture(x))
    stop("Currently only plotting univariate, discrete or continuous, distributions are supported.")

  plotFuns <- c("pdf","cdf","quantile","survival","hazard","cumhazard","all")
  fun = unique(plotFuns[charmatch(fun, plotFuns)])
  if("all" %in% fun)
    fun = plotFuns[-7]

  if(any(is.na(fun)))
    stop("Function unrecognised, should be one of: ", paste0(plotFuns,collapse=","))

  if("cdf" %in% fun & !x$isCdf){
    message("This distribution does not have a cdf expression. Use the
            FunctionImputation decorator to impute a numerical cdf.")
    fun = fun[!(fun %in% c("cdf", "survival", "hazard","cumhazard"))]
  }

  if("pdf" %in% fun & !x$isPdf){
    message("This distribution does not have a pdf expression. Use the
            FunctionImputation decorator to impute a numerical pdf.")
    fun = fun[!(fun %in% c("pdf", "hazard"))]
  }

  if("quantile" %in% fun & !x$isQuantile){
    message("This distribution does not have a quantile expression. Use the
            FunctionImputation decorator to impute a numerical quantile.")
    fun = fun[!(fun %in% c("quantile"))]
  }

  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################

  if(testDiscrete(x) & x$support()$length() != Inf){
    plotStructure <- data.table::data.table(points = x$support()$elements())
    plotStructure$cdf <- x$cdf(plotStructure$points)
  } else {
    plotStructure <- data.table::data.table(cdf = seq(0,1,length.out = npoints))
    plotStructure$points <- x$quantile(plotStructure$cdf)
    plotStructure <- plotStructure[,2:1]

    if(testDiscrete(x))
      plotStructure <- stats::aggregate(cdf ~ points, plotStructure, max)
  }

  plotStructure$pdf <- x$pdf(plotStructure$points)

  if("survival" %in% fun)
    plotStructure$survival <- 1 - plotStructure$cdf
  if("hazard" %in% fun)
    plotStructure$hazard <- plotStructure$pdf/(1 - plotStructure$cdf)
  if("cumhazard" %in% fun)
    plotStructure$cumhazard <- -log(1 - plotStructure$cdf)


  #######################################################################
  #######                     graphical parameters                #######
  #######################################################################

  if(length(fun) == 1)
    ask = arrange = FALSE

  if(plot){
    if(ask | arrange){
      def.par <- graphics::par(no.readonly = TRUE)
      graphics::par(ask = ask)
    }

    if(arrange & !ask){
      data = 1:length(fun)
      if(length(fun) == 3 | length(fun) == 5) data = c(data, 0)

      n = switch(length(fun),
                 "1" = list(nrow = 1, ncol = 1),
                 "2" = list(nrow = 1, ncol = 2),
                 "3" = list(nrow = 2, ncol = 2),
                 "4" = list(nrow = 2, ncol = 2),
                 "5" = list(nrow = 2, ncol = 3),
                 "6" = list(nrow = 2, ncol = 3)
      )

      graphics::layout(do.call(matrix, c(list(byrow = TRUE, data = data), n)))

    }

    if(testContinuous(x))
      .plot_continuous(fun, plotStructure, x$strprint(), ...)
    else if(testDiscrete(x))
      .plot_discrete(fun, plotStructure, x$strprint(), ...)

    if(ask | arrange)
      graphics::par(def.par)
  }

  invisible(data.table::data.table(plotStructure))
}

