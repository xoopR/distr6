.lines_continuous <- function(fun,plotStructure,...){
  if("pdf" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$pdf,...)

  if("cdf" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$cdf,...)

  if("quantile" %in% fun)
    lines(x = plotStructure$cdf, y = plotStructure$points,...)

  if("survival" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$survival,...)

  if("hazard" %in% fun)
    lines(x = plotStructure$points, y = plotStructure$hazard,...)

  if("cumhazard" %in% fun)
    lines(plotStructure$points, y = plotStructure$cumhazard,...)
}
