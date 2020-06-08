.lines_continuous <- function(fun, plotStructure, ...) {
  if ("pdf" %in% fun) {
    graphics::lines(x = plotStructure$points, y = plotStructure$pdf, ...)
  }

  if ("cdf" %in% fun) {
    graphics::lines(x = plotStructure$points, y = plotStructure$cdf, ...)
  }

  if ("quantile" %in% fun) {
    graphics::lines(x = plotStructure$cdf, y = plotStructure$points, ...)
  }

  if ("survival" %in% fun) {
    graphics::lines(x = plotStructure$points, y = plotStructure$survival, ...)
  }

  if ("hazard" %in% fun) {
    graphics::lines(x = plotStructure$points, y = plotStructure$hazard, ...)
  }

  if ("cumhazard" %in% fun) {
    graphics::lines(plotStructure$points, y = plotStructure$cumhazard, ...)
  }
}
