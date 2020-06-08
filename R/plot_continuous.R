.plot_continuous <- function(fun, plotStructure, name, ...) {
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
      list(x = plotStructure$points, y = plotStructure$pdf, type = "l"),
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

    plots$cdf <- c(
      list(x = plotStructure$points, y = plotStructure$cdf, type = "l"),
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

    plots$quantile <- c(
      list(x = plotStructure$cdf, y = plotStructure$points, type = "l"),
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

    plots$survival <- c(
      list(x = plotStructure$points, y = plotStructure$survival, type = "l"),
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
      list(x = plotStructure$points, y = plotStructure$hazard, type = "l"),
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

    plots$cumhazard <- c(
      list(x = plotStructure$points, y = plotStructure$cumhazard, type = "l"),
      args
    )
  }

  if (length(fun) > 1) {
    plots <- plots[match(fun, names(plots))]
  }

  lapply(plots, function(x) do.call(graphics::plot, x))
}
