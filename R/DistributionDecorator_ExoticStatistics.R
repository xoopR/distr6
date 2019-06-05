#' @title Exotic Statistical Methods for Distributions
#'
#' @description Further functionality to distribution objects for numerical statistical
#'   methods that can be considered more exotic than core, such as survival modelling
#'   and p-norms.
#' @name ExoticStatistics
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' \code{R62S3} \tab logical \tab If TRUE (default), S3 methods are added for decorators in construction.
#' }
#'
#' @section Public Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input -> Output} \tab \strong{Details} \cr
#' \code{cdfAntiDeriv(lower = self$inf(), upper = self$sup())} \tab numeric x numeric -> numeric \tab
#' Anti-derivative of cdf evaluated between lower and upper. \cr
#' \code{survivalAntiDeriv(lower = self$inf(), upper = self$sup())} \tab numeric x numeric -> numeric \tab
#' Anti-derivative of survival function evaluated between lower and upper. \cr
#' \code{survival(x1, log = FALSE)} \tab numeric x logical -> numeric \tab
#' Survival function evaluated at x1, log(survival) if log = TRUE. \cr
#' \code{hazard(x1, log = FALSE)} \tab numeric x logical -> numeric \tab
#' Hazard function evaluated at x1, log(hazard) if log = TRUE. \cr
#' \code{cumHazard(x1, log = FALSE)} \tab numeric x logical -> numeric \tab
#' Cumulative hazard function evaluated at x1, log(cumHazard) if log = TRUE. \cr
#' \code{cdfPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab integer x numeric x numeric -> numeric \tab
#' The pth norm of the cumulative distribution function, evaluated between limits. \cr
#' \code{pdfPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab integer x numeric x numeric -> numeric \tab
#' The pth norm of the probability density function, evaluated between limits. \cr
#' \code{survivalPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab integer x numeric x numeric -> numeric \tab
#' The pth norm of the survival function, evaluated between limits.
#' }
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @seealso \code{\link{DistributionDecorator}}
#'
#' @examples
#' x = Exponential$new()
#' decorate(x, ExoticStatistics)
#' x$survival(1)
#'
#' @examples
#' x = Exponential$new(decorators = ExoticStatistics)
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
ExoticStatistics$set("public", "cdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$cdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "pdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$pdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "survivalPNorm", function(p = 2, lower = self$inf(),
                                                         upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$survival, p, lower, upper))
}) # NEEDS TESTING
