#' @title Generalised P-Norm
#' @description Calculate the p-norm of any function between given limits. Given by,
#' \deqn{(\int_S |f|^p d\mu)^1/p}
#' @usage generalPNorm(fun, p, lower, upper)
#' @param fun function to calculate the p-norm of.
#' @param p the pth norm to calculate
#' @param lower lower bound for the integral
#' @param upper upper bound for the integral
#'
#' @examples
#' generalPNorm(Exponential$new()$pdf,2,0,10)
#'
#' @export
generalPNorm <- function(fun, p, lower, upper){
  message(.distr6$message_numeric)
  return((stats::integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
}
