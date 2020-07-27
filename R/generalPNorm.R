#' @title Generalised P-Norm
#' @description Calculate the p-norm of any function between given limits.
#' @details The p-norm of a continuous function \eqn{f} is given by,
#' \deqn{(\int_S |f|^p d\mu)^{1/p}}
#' where \eqn{S} is the function support. And for a discrete function by
#' \deqn{\sum_i (x_{i + 1} - x_i) * |f(x_i)|^p}
#' where \eqn{i} is over a given range.
#'
#' The p-norm is calculated numerically using the \code{integrate} function and therefore results
#' are approximate only.
#'
#' @param fun function to calculate the p-norm of.
#' @param p the pth norm to calculate
#' @param lower lower bound for the integral
#' @param upper upper bound for the integral
#' @param range if discrete then range of the function to sum over
#'
#' @examples
#' generalPNorm(Exponential$new()$pdf, 2, 0, 10)
#' @return Returns a numeric value for the p norm of a function evaluated between given limits.
#'
#' @export
generalPNorm <- function(fun, p, lower, upper, range = NULL) {
  if (!is.null(range)) {
    return(sum(abs(fun(range[1:(length(range) - 1)]))^p * diff(range)))
  } else {
    message(.distr6$message_numeric)
    return((stats::integrate(f = function(x) abs(fun(x))^p, lower, upper)$value)^(1 / p)) # nolint
  }
}
