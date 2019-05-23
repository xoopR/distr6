#' @title Exotic Statistical Methods for Distributions
#'
#' @description Further functionality to distribution objects for numerical statistical
#'   methods that can be considered more exotic than core, such as survival modelling
#'   and p-norms.
#' @name ExoticStatistics
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution. See the help pages for the individual
#'  CoreStatistics methods to learn more.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @seealso \code{\link{DistributionDecorator}}
#'
#' @examples
#' x = Exponential$new()
#' decorate(x, ExoticStatistics, R62S3 = FALSE)
#' x$survival(1)
#'
#' @examples
#' x = Exponential$new(decorators = ExoticStatistics, R62S3 = FALSE)
#' x$survival(4)
NULL


#' @export
ExoticStatistics <- R6::R6Class("ExoticStatistics", inherit = DistributionDecorator)
ExoticStatistics$set("public", "cdfAntiDeriv", function(lower = self$inf(),
                                                        upper = self$sup()){
  return(self$cdfPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)
ExoticStatistics$set("public", "survivalAntiDeriv", function(lower = self$inf(),
                                                             upper = self$sup()) {
  return(self$survivalPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)
ExoticStatistics$set("public", "logCdf", function(x1) {
}) # TO DO
ExoticStatistics$set("public", "generalisedIntegral", function() {
}) # TO DO
ExoticStatistics$set("public", "survival", function(x1, log = FALSE) {
  if(!is.null(self$cdf(x1))){
    if(log)
      return(log(1 - self$cdf(x1)))
    else
      return(1 - self$cdf(x1))
  } else {
    message(.distr6$message_numeric)
    surv = integrate(self$pdf, x1, Inf)$value
    if(log)
      return(log(surv))
    else
      return(surv)
  }
}) # DONE
ExoticStatistics$set("public", "hazard", function(x1, log=FALSE) {
  if(!is.null(self$pdf(x1)))
    pdf = self$pdf(x1)
  else if(!is.null(self$cdf(x1)))
    pdf = deriv(y~self$cdf(x1),"x1")

  surv = self$survival(x1)

  haz = pdf/surv

  if(log)
    return(log(haz))
  else
    return(haz)
}) # IN PROGRESS
ExoticStatistics$set("public", "cumHazard", function(x1, log=FALSE) {
  if(!log){
    return(-log(self$survival(x1)))
  }
}) # IN PROGRESS
ExoticStatistics$set("public", "generalPNorm", function(fun, p, lower, upper){
  if(testContinuous(self)){
    warning("Results from numerical integration are approximate only, better results may be available.")
    return((integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
  }
}) # NEEDS TESTING
ExoticStatistics$set("public", "cdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  return(self$generalPNorm(self$cdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "pdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  return(self$generalPNorm(self$pdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "survivalPNorm", function(p = 2, lower = self$inf(),
                                                         upper = self$sup()) {
  return(self$generalPNorm(self$survival, p, lower, upper))
}) # NEEDS TESTING
