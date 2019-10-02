#' @export
lines.Distribution <- function(x, fun, npoints = 3000,...){

  #######################################################################
  #######                         validations                     #######
  #######################################################################

  plotFuns <- c("pdf","cdf","quantile","survival","hazard","cumhazard")
  # check user input plot names are correct
  if(!all(fun %in% plotFuns))
    stop("invalid plot function")

  if("cdf" %in% fun & !x$.__enclos_env__$private$.isCdf){
    message("This distribution does not have a cdf expression. Use the
            FunctionImputation decorator to impute a numerical cdf.")
    fun = fun[!(fun %in% c("cdf", "survival", "hazard","cumhazard"))]
  }

  if("pdf" %in% fun & !x$.__enclos_env__$private$.isPdf){
    message("This distribution does not have a pdf expression. Use the
            FunctionImputation decorator to impute a numerical pdf.")
    fun = fun[!(fun %in% c("pdf", "hazard"))]
  }

  if("quantile" %in% fun & !x$.__enclos_env__$private$.isQuantile){
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
      plotStructure <- aggregate(cdf ~ points, plotStructure, max)
  }

  plotStructure$pdf <- x$pdf(plotStructure$points)

  if("survival" %in% fun)
    plotStructure$survival <- 1 - plotStructure$cdf
  if("hazard" %in% fun)
    plotStructure$hazard <- plotStructure$pdf/(1 - plotStructure$cdf)
  if("cumhazard" %in% fun)
    plotStructure$cumhazard <- -log(1 - plotStructure$cdf)

  if(testContinuous(x)){
   .plot_continuous_lines(fun,plotStructure,...)}
  # discrete case
  if(testDiscrete(x)){
   .plot_discrete_lines(fun,plotStructure,...)}

  invisible(plotStructure)
}



# FUN_TWO: continuous distribution: adding (a) line(s)
.plot_continuous_lines <- function(fun,plotStructure,...){
  if("pdf" %in% fun)
    lines(data.frame(x = plotStructure$points, y = plotStructure$pdf),...)
  if("cdf" %in% fun)
    lines(data.frame(x = plotStructure$points, y = plotStructure$cdf),...)
  if("quantile" %in% fun)
    lines(data.frame(x = plotStructure$cdf, y = plotStructure$points),...)
  if("survival" %in% fun)
    lines(data.frame(x = plotStructure$points, y = plotStructure$survival),...)
  if("hazard" %in% fun)
    lines(data.frame(x = plotStructure$points, y = plotStructure$hazard),...)
  if("cumhazard" %in% fun)
    lines(data.frame(plotStructure$points, y = plotStructure$cumhazard),...)
  }

# FUN_FOUR: discrete distribution: adding (a) line(s)
.plot_discrete_lines <- function(fun,plotStructure,...){
  if("pdf" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$pdf,
          type = "h",...)

  if("cdf" %in% fun){
      points(x = plotStructure$points, y = plotStructure$cdf, pch = 16,...)
      segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                y0 = plotStructure$cdf,...)

  }

  if("quantile" %in% fun){
      points(x = plotStructure$cdf, y = plotStructure$points, pch = 16,...)
      segments(x0 = plotStructure$cdf, y0 = plotStructure$points,
               y1 = plotStructure$points+1,...)
  }
  if("survival" %in% fun){
    points(x = plotStructure$points, y = plotStructure$survival, pch = 16,...)
    segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
             y0 = plotStructure$survival,...)
  }

  if("hazard" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$hazard, type = "h",...)

  if ("cumhazard" %in% fun){
    points(x = plotStructure$points, y = plotStructure$cumhazard, pch = 16,...)
    segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
             y0 = plotStructure$cumhazard,...)
  }

}




