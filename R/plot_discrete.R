.plot_discrete <- function(fun, plotStructure, name, ...) {
  plots <- list()
  dots <- as.list(match.call(expand.dots = FALSE))$...
  common_args <- dots[!grepl("_", names(dots), fixed = TRUE)]

  if ("pdf" %in% fun) {
    args <- dots[grepl("^pdf", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 5, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "Pdf")
    if (!("xlab" %in% names(args))) args$xlab <- "x"
    if (!("ylab" %in% names(args))) args$ylab <- "f(x)"

    plots$pdf <- c(
      list(x = plotStructure$points, y = plotStructure$pdf, type = "h"),
      args
    )
  }

  if ("cdf" %in% fun) {
    args <- dots[grepl("^cdf", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 5, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "Cdf")
    if (!("xlab" %in% names(args))) args$xlab <- "x"
    if (!("ylab" %in% names(args))) args$ylab <- "F(x)"

    plots$cdf$plot <- c(
      list(x = plotStructure$points, y = plotStructure$cdf, type = "n"),
      args
    )
    plots$cdf$points <- c(
      list(x = plotStructure$points, y = plotStructure$cdf, pch = 16),
      args
    )
    plots$cdf$segments <- c(
      list(
        x0 = plotStructure$points,
        x1 = c(
          plotStructure$points[2:length(plotStructure$points)],
          plotStructure$points[length(plotStructure$points)] + 1
        ),
        y0 = plotStructure$cdf
      ),
      args
    )
  }

  if ("quantile" %in% fun) {
    args <- dots[grepl("^quantile", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 10, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "Quantile")
    if (!("xlab" %in% names(args))) args$xlab <- "p"
    if (!("ylab" %in% names(args))) args$ylab <- parse(text = "F^-1*(p)")

    plots$quantile$plot <- c(
      list(x = plotStructure$cdf, y = plotStructure$points, type = "n"),
      args
    )
    plots$quantile$points <- c(
      list(x = plotStructure$cdf, y = plotStructure$points, pch = 16),
      args
    )
    plots$quantile$segments <- c(
      list(
        x0 = plotStructure$cdf,
        y0 = plotStructure$points,
        y1 = c(
          plotStructure$points[2:length(plotStructure$points)],
          plotStructure$points[length(plotStructure$points)] + 1
        )
      ),
      args
    )
  }

  if ("survival" %in% fun) {
    args <- dots[grepl("^survival", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 10, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "Survival")
    if (!("xlab" %in% names(args))) args$xlab <- "x"
    if (!("ylab" %in% names(args))) args$ylab <- parse(text = "S(x)")

    plots$survival$plot <- c(
      list(x = plotStructure$points, y = plotStructure$survival, type = "n"),
      args
    )
    plots$survival$points <- c(
      list(x = plotStructure$points, y = plotStructure$survival, pch = 16),
      args
    )
    plots$survival$segments <- c(
      list(
        x0 = plotStructure$points,
        x1 = c(
          plotStructure$points[2:length(plotStructure$points)],
          plotStructure$points[length(plotStructure$points)] + 1
        ),
        y0 = plotStructure$survival
      ),
      args
    )
  }

  if ("cumhazard" %in% fun) {
    args <- dots[grepl("^cumhazard", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 11, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "CumHazard")
    if (!("xlab" %in% names(args))) args$xlab <- "x"
    if (!("ylab" %in% names(args))) args$ylab <- parse(text = "H(x)")

    plots$cumhazard$plot <- c(
      list(x = plotStructure$points, y = plotStructure$cumhazard, type = "n"),
      args
    )
    plots$cumhazard$points <- c(
      list(x = plotStructure$points, y = plotStructure$cumhazard, pch = 16),
      args
    )
    plots$cumhazard$segments <- c(
      list(
        x0 = plotStructure$points,
        x1 = c(
          plotStructure$points[2:length(plotStructure$points)],
          plotStructure$points[length(plotStructure$points)] + 1
        ),
        y0 = plotStructure$cumhazard
      ),
      args
    )
  }

  if ("hazard" %in% fun) {
    args <- dots[grepl("^hazard", names(dots))]
    if (length(args) > 0) names(args) <- substr(names(args), 8, 1000)
    args <- c(args, common_args)
    args <- args[unique(names(args))]

    if (!("main" %in% names(args))) args$main <- paste(name, "Hazard")
    if (!("xlab" %in% names(args))) args$xlab <- "x"
    if (!("ylab" %in% names(args))) args$ylab <- parse(text = "h(x)")

    plots$hazard <- c(
      list(x = plotStructure$points, y = plotStructure$hazard, type = "h"),
      args
    )
  }

  if (length(fun) > 1) {
    plots <- plots[match(fun, names(plots))]
  }

  lapply(plots, function(x) {
    if (length(x) != 3) {
      do.call(graphics::plot, x)
    } else {
      do.call(graphics::plot, x$plot)
      do.call(graphics::points, x$points)
      do.call(graphics::segments, x$segments)
    }
  })
}
