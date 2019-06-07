#' @title Exotic Statistical Methods for Distributions
#'
#' @description Further functionality to distribution objects for numerical statistical
#'   methods that can be considered more exotic than core, such as survival modelling
#'   and p-norms.
#'
#' @name ExoticStatistics
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{survival(x1, log = FALSE)} \tab Survival function \tab \code{\link{survival}} \cr
#' \code{hazard(x1, log = FALSE)} \tab Hazard function \tab \code{\link{hazard}} \cr
#' \code{cumHazard(x1, log = FALSE)} \tab Cumulative hazard function \tab \code{\link{cumHazard}} \cr
#' \code{cdfAntiDeriv(lower = self$inf(), upper = self$sup())} \tab Anti-derivative of cdf \tab \code{\link{cdfAntiDeriv}} \cr
#' \code{survivalAntiDeriv(lower = self$inf(), upper = self$sup())} \tab Anti-derivative of survival function \tab \code{\link{survivalAntiDeriv}} \cr
#' \code{cdfPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab P-norm of cdf \tab \code{\link{cdfPNorm}} \cr
#' \code{pdfPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab P-norm of pdf \tab \code{\link{pdfPNorm}} \cr
#' \code{survivalPNorm(p = 2, lower = self$inf(), upper = self$sup())} \tab P-norm of survival function \tab \code{\link{survivalPNorm}} \cr
#' }
#'
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations. See below for the methods added to a distribution after decorating with
#'  \code{ExoticStatistics}.
#'
#' @seealso \code{\link{DistributionDecorator}} and \code{\link{CoreStatistics}}
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

#' @title Cumulative Distribution Function Anti-Derivative
#' @name cdfAntiDeriv
#' @description The anti-derivative of the cumulative distribution function between given limits or
#' over the full support.
#'
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The cdf anti-derivative is defined by
#' \deqn{acdf(a, b) = \int_a^b F_X(x) dx}
#' where X is the distribution, F_X is the cdf of the distribution X and a,b are the limits of integration.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
NULL
ExoticStatistics$set("public", "cdfAntiDeriv", function(lower = self$inf(),
                                                        upper = self$sup()){
  return(self$cdfPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)

#' @title Survival Function Anti-Derivative
#' @name survivalAntiDeriv
#' @description The anti-derivative of the survival function between given limits or
#' over the full support.
#'
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The survival anti-derivative is defined by
#' \deqn{as(a, b) = \int_a^b S_X(x) dx}
#' where X is the distribution, S_X is the survival function of the distribution X and a,b are the
#' limits of integration.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
NULL
ExoticStatistics$set("public", "survivalAntiDeriv", function(lower = self$inf(),
                                                             upper = self$sup()) {
  return(self$survivalPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)

#' @title Survival Function
#' @name survival
#' @description The survival function of a probability distribution is the probability of surviving
#' after a point x.
#'
#' @param x1 Point to evaluate the survival function at.
#' @param log logical, if TRUE then the (natural) logarithm of the survival function is returned.
#'
#' @details The survival function is defined by
#' \deqn{S_X(x) = P(X \geq x) = 1 - F_X(x) \int_x^{Sup(X)} f_X(x) dx}
#' where X is the distribution, \eqn{S_X} is the survival function, \eqn{F_X} is the cdf, \eqn{f_X} is the pdf and
#' Sup(X) is the supremum of the distribution X.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
NULL
ExoticStatistics$set("public", "survival", function(x1, log = FALSE) {
  if(!is.null(self$cdf(x1))){
    if(log)
      return(log(1 - self$cdf(x1)))
    else
      return(1 - self$cdf(x1))
  } else {
    message(.distr6$message_numeric)
    surv = integrate(self$pdf, x1, self$sup())$value
    if(log)
      return(log(surv))
    else
      return(surv)
  }
}) # DONE

#' @title Hazard Function
#' @name hazard
#' @description The hazard function of a probability distribution is the risk of instantaneous event at
#' a point x.
#'
#' @param x1 Point to evaluate the hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the hazard function is returned.
#'
#' @details The hazard function is defined analytically by
#' \deqn{h_X(x) = \frac{f_X}{S_X}}
#' where X is the distribution, \eqn{S_X} is the survival function and \eqn{f_X} is the pdf.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
ExoticStatistics$set("public", "hazard", function(x1, log=FALSE) {
  if(!is.null(self$pdf(x1)))
    pdf = self$pdf(x1)
  else if(!is.null(self$cdf(x1))){
    message(.distr6$message_numeric)
    pdf = deriv(y~self$cdf(x1),"x1")
  }

  surv = self$survival(x1)

  haz = pdf/surv

  if(log)
    return(log(haz))
  else
    return(haz)
})

#' @title Cumulative Hazard Function
#' @name cumHazard
#' @description The cumulative hazard function of a probability distribution is the anti-derivative of
#' the hazard function.
#'
#' @param x1 Point to evaluate the cumulative hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the cumulative hazard function is returned.
#'
#' @details The cumulative hazard function is defined analytically by
#' \deqn{H_X(x) = -log(S_X)}
#' where X is the distribution and \eqn{S_X} is the survival function.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
ExoticStatistics$set("public", "cumHazard", function(x1, log=FALSE) {
  if(!log){
    return(-log(self$survival(x1)))
  } else
    return(log(-log(self$survival(x1))))
})

#' @title Cumulative Distribution Function P-Norm
#' @name cdfPNorm
#' @description The p-norm of the cdf evaluated between given limits or over the whole support.
#'
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the cdf is defined by
#' \deqn{\int_a^b |F_X|^p d\mu)^1/p}
#' where X is the distribution, \eqn{F_X} is the cdf and a,b are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
ExoticStatistics$set("public", "cdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$cdf, p, lower, upper))
}) # NEEDS TESTING

#' @title Probability Density Function P-Norm
#' @name pdfPNorm
#' @description The p-norm of the pdf evaluated between given limits or over the whole support.
#'
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the pdf is defined by
#' \deqn{\int_a^b |f_X|^p d\mu)^1/p}
#' where X is the distribution, \eqn{f_X} is the pdf and a,b are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
ExoticStatistics$set("public", "pdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$pdf, p, lower, upper))
}) # NEEDS TESTING

#' @title Survival Function P-Norm
#' @name survivalPNorm
#' @description The p-norm of the survival function evaluated between given limits or over the whole support.
#'
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the survival function is defined by
#' \deqn{\int_a^b |S_X|^p d\mu)^1/p}
#' where X is the distribution, \eqn{S_X} is the survival function and a,b are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' an R6 method.
#'
#' @export
ExoticStatistics$set("public", "survivalPNorm", function(p = 2, lower = self$inf(),
                                                         upper = self$sup()) {
  if(testContinuous(self))
    return(generalPNorm(self$survival, p, lower, upper))
}) # NEEDS TESTING
