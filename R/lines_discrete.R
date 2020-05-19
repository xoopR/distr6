.lines_discrete <- function(fun, plotStructure, ...) {
  if ("pdf" %in% fun) {
    graphics::lines(
      x = plotStructure$points, y = plotStructure$pdf,
      type = "h", ...
    )
  }

  if ("cdf" %in% fun) {
    graphics::points(x = plotStructure$points, y = plotStructure$cdf, pch = 16, ...)
    graphics::segments(
      x0 = plotStructure$points,
      x1 = c(
        plotStructure$points[2:length(plotStructure$points)],
        plotStructure$points[length(plotStructure$points)] + 1
      ),
      y0 = plotStructure$cdf, ...
    )

  }

  if ("quantile" %in% fun) {
    graphics::points(x = plotStructure$cdf, y = plotStructure$points, pch = 16, ...)
    graphics::segments(
      x0 = plotStructure$cdf,
      y0 = plotStructure$points,
      y1 = c(
        plotStructure$points[2:length(plotStructure$points)],
        plotStructure$points[length(plotStructure$points)] + 1
      ), ...
    )
  }

  if ("survival" %in% fun) {
    graphics::points(x = plotStructure$points, y = plotStructure$survival, pch = 16, ...)
    graphics::segments(
      x0 = plotStructure$points,
      x1 = c(
        plotStructure$points[2:length(plotStructure$points)],
        plotStructure$points[length(plotStructure$points)] + 1
      ),
      y0 = plotStructure$survival, ...
    )
  }

  if ("hazard" %in% fun) {
    graphics::lines(x = plotStructure$points, y = plotStructure$hazard, type = "h", ...)
  }

  if ("cumhazard" %in% fun) {
    graphics::points(x = plotStructure$points, y = plotStructure$cumhazard, pch = 16, ...)
    graphics::segments(
      x0 = plotStructure$points,
      x1 = c(
        plotStructure$points[2:length(plotStructure$points)],
        plotStructure$points[length(plotStructure$points)] + 1
      ),
      y0 = plotStructure$cumhazard, ...
    )
  }
}
