.plot_multivariate <- function(dist, fun = c("pdf", "cdf"), npoints) {

  assert_pkgload("plotly")

  fun <- match.arg(fun)
  n <- round(sqrt(npoints))

  if (testCountablyFinite(dist$properties$support)) {
    min <- min(as.numeric(dist$properties$support$wrappedSets[[1]]$elements))
    max <- max(as.numeric(dist$properties$support$wrappedSets[[1]]$elements))

    if (getR6Class(dist) == "EmpiricalMV") {
      x <- seq.int(min, max, length.out = n)
    } else {
      x <- seq.int(min, max)
      n <- length(x)
    }

  } else {
    x <- unlist(dist$rand(n)[, 1])
  }

  if (fun == "pdf") {
    f <- dist$pdf(data = expand.grid(x, x))
  } else {
    f <- dist$cdf(data = expand.grid(x, x))
  }

  # plotStructure <- data.table(x, x, f)

  f <- matrix(f, nrow = n, ncol = n)

  plotly::add_surface(plotly::plot_ly(x = x, y = x, z = f))

  # return(plotStructure)
}
