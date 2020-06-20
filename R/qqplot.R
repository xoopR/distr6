#' @title Quantile-Quantile Plots for distr6 Objects
#' @author Chijing Zeng
#'
#' @description Quantile-quantile plots are used to compare a "theoretical" or
#' empirical distribution to a reference distribution. They can also compare the quantiles of two
#' reference distributions.
#'
#' @param x \code{distr6} object or numeric vector.
#' @param y \code{distr6} object or numeric vector.
#' @param npoints number of evaluation points.
#' @param plot logical; if TRUE (default), figures are displayed in the plot window; otherwise a
#' [data.table::data.table] of points and calculated values is returned.
#' @param idline logical; if TRUE (default), the line \eqn{y = x} is plotted
#' @param ... graphical parameters.
#'
#' @details If \code{x} or \code{y} are given as numeric vectors then they are first passed to the
#' [Empirical] distribution. The [Empirical] distribution is a discrete distribution so quantiles
#' are equivalent to the the Type 1 method in \code{\link[stats]{quantile}}.
#'
#'
#' @seealso \code{\link{plot.Distribution}} for plotting a \code{distr6} object.
#'
#' @examples
#' qqplot(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15))
#' qqplot(rt(200, df = 5), rt(300, df = 5),
#'   main = "QQ-Plot", xlab = "t-200",
#'   ylab = "t-300"
#' )
#' qqplot(Normal$new(mean = 2), rnorm(100, mean = 3))
#' @export
qqplot <- function(x, y, npoints = 3000, idline = TRUE, plot = TRUE, ...) {

  # random number generator: sample quantiles from 0 to 1 (cdf)
  points <- seq(0, 1, length.out = npoints)

  if (is.numeric(x)) {
    x <- Empirical$new(x)
  }
  if (is.numeric(y)) {
    y <- Empirical$new(y)
  }

  assertDistributionList(list(x, y), "x and y should both be distributions")

  quantile.X <- x$quantile(points)
  quantile.Y <- y$quantile(points)
  #
  #   nx <- length(quantile.X)
  #   ny <- length(quantile.Y)
  #
  #   if(nx == ny)
  #     print(TRUE)
  #   else
  #     print(FALSE)
  #
  #   if (nx != ny) {
  #     ppoints.x <- ppoints(nx, a = pos)
  #     ppoints.y <- ppoints(ny, a = pos)
  #     if (nx > ny)
  #       quantile.X <- approx(ppoints.x, quantile.X, xout = ppoints.y, rule = 2)$y
  #     else
  #       quantile.Y <- approx(ppoints.y, quantile.Y, xout = ppoints.x, rule = 2)$y
  #   }

  # Now plot!

  ## Quantile vs Quantile##
  if (plot) {
    graphics::plot(x = quantile.X, y = quantile.Y, type = "p", ...)

    ## Identity Line##
    if (idline) {
      graphics::abline(a = 0, b = 1, col = "blue", lty = 2, lwd = 2)
    }
  }

  #
  #     ##Confidence Lines##
  #     x0 <- quantile.X
  #     y0 <- quantile.Y
  #
  #     x0[is.infinite(x0)] <- NA
  #     y0[is.infinite(y0)] <- NA
  #
  #     quantile.df <- data.frame(x0, y0)
  #     quantile.df_no_na <- na.omit(quantile.df)
  #
  #     coef <- coef(MASS::rlm(y0 ~ x0, data = quantile.df_no_na))
  #     a <- coef[1]
  #     b <- coef[2]
  #
  #     conf <- if (conf == FALSE) 0.95 else conf
  #     xx <- Normal$new()$quantile(1 - (1 - conf)/2)
  #
  #     SE <- (b / Normal$new()$pdf(Normal$new()$quantile(ppoints(npoints)))) *
  #      sqrt(ppoints(npoints)*(1 - ppoints(npoints))/npoints)
  #
  #     fit.value <- a + b*x0
  #     upper <- fit.value + (xx*SE)
  #     lower <- fit.value - (xx*SE)

  # if (withConf != FALSE) {
  #   lines(x0, upper, lty=3, col = "orange", lwd = 2)
  #   lines(x0, lower, lty=3, col = "orange", lwd = 2)
  # }

  invisible(list(x = quantile.X, y = quantile.Y))

}
