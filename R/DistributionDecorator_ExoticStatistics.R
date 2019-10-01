#' @name ExoticStatistics
#'
#' @title Exotic Statistical Methods for Distributions
#'
#' @description This decorator adds methods for more complex statistical methods including p-norms,
#' survival and hazard functions and anti-derivatives.
#'
#' @details Decorator objects add functionality to the given Distribution object by copying methods
#' in the decorator environment to the chosen Distribution environment. See the 'Added Methods' section
#' below to find details of the methods that are added to the Distribution. Methods already
#' present in the distribution are not overwritten by the decorator.
#'
#' Use \code{\link{decorate}} to decorate a Distribution.
#'
#' Methods in this decorator may use numerical approximations and therefore better results may be available
#' from analytic computations.
#'
#' @section Constructor: ExoticStatistics$new(distribution)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{survival(x1, log = FALSE)} \tab Survival function \tab \code{\link{survival}} \cr
#' \code{hazard(x1, log = FALSE)} \tab Hazard function \tab \code{\link{hazard}} \cr
#' \code{cumHazard(x1, log = FALSE)} \tab Cumulative hazard function \tab \code{\link{cumHazard}} \cr
#' \code{cdfAntiDeriv(lower = NULL, upper = NULL))} \tab Anti-derivative of cdf \tab \code{\link{cdfAntiDeriv}} \cr
#' \code{survivalAntiDeriv(lower = NULL, upper = NULL)} \tab Anti-derivative of survival function \tab \code{\link{survivalAntiDeriv}} \cr
#' \code{cdfPNorm(p = 2, lower = NULL, upper = NULL)} \tab P-norm of cdf \tab \code{\link{cdfPNorm}} \cr
#' \code{pdfPNorm(p = 2, lower = NULL, upper = NULL)} \tab P-norm of pdf \tab \code{\link{pdfPNorm}} \cr
#' \code{survivalPNorm(p = 2, lower = NULL, upper = NULL)} \tab P-norm of survival function \tab \code{\link{survivalPNorm}} \cr
#' }
#'
#' @seealso \code{\link{decorate}}, \code{\link{listDecorators}}
#'
#' @return Returns a decorated R6 object inheriting from class SDistribution with the methods listed below
#' added to the SDistribution methods.
#'
#' @examples
#' x = Exponential$new()
#' decorate(x, ExoticStatistics)
#' x$survival(1)
#'
#' @examples
#' x = Exponential$new(decorators = ExoticStatistics)
#' x$survival(4)
#'
#' @export
NULL
ExoticStatistics <- R6::R6Class("ExoticStatistics", inherit = DistributionDecorator)
.distr6$decorators <- append(.distr6$decorators, list(ExoticStatistics = ExoticStatistics))
#-------------------------------------------------------------
# Public Methods - cdfAntiDeriv
#-------------------------------------------------------------
#' @title Cumulative Distribution Function Anti-Derivative
#' @name cdfAntiDeriv
#' @description The anti-derivative of the cumulative distribution function between given limits or
#' over the full support.
#'
#' @usage cdfAntiDeriv(object, lower = NULL, upper = NULL)
#' @section R6 Usage: $cdfAntiDeriv(lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The cdf anti-derivative is defined by
#' \deqn{acdf(a, b) = \int_a^b F_X(x) dx}
#' where X is the distribution, \eqn{F_X} is the cdf of the distribution \eqn{X} and \eqn{a, b} are the limits of integration.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @return Antiderivative of the cdf evaluated between limits as a numeric.
#'
#' @export
NULL
ExoticStatistics$set("public", "cdfAntiDeriv", function(lower = NULL, upper = NULL){
  if(is.null(lower)) lower <- self$inf()
  if(is.null(upper)) upper <- self$sup()
  return(self$cdfPNorm(p = 1, lower, upper))
})

#-------------------------------------------------------------
# Public Methods - survivalAntiDeriv
#-------------------------------------------------------------
#' @title Survival Function Anti-Derivative
#' @name survivalAntiDeriv
#' @description The anti-derivative of the survival function between given limits or
#' over the full support.
#'
#' @usage survivalAntiDeriv(object, lower = NULL, upper = NULL)
#' @section R6 Usage: $survivalAntiDeriv(lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The survival anti-derivative is defined by
#' \deqn{as(a, b) = \int_a^b S_X(x) dx}
#' where X is the distribution, \eqn{S_X} is the survival function of the distribution \eqn{X} and \eqn{a, b} are the
#' limits of integration.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @return Antiderivative of the survival function evaluated between limits as a numeric.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
ExoticStatistics$set("public", "survivalAntiDeriv", function(lower = NULL, upper = NULL) {
  if(is.null(lower)) lower <- self$inf()
  if(is.null(upper)) upper <- self$sup()
  return(self$survivalPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)

#-------------------------------------------------------------
# Public Methods - survival
#-------------------------------------------------------------
#' @title Survival Function
#' @name survival
#' @description The survival function of a probability distribution is the probability of surviving
#' after a point x.
#'
#' @usage survival(object, x1, log = FALSE)
#' @section R6 Usage: $survival(x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the survival function at.
#' @param log logical, if TRUE then the (natural) logarithm of the survival function is returned.
#'
#' @details The survival function is defined by
#' \deqn{S_X(x) = P(X \ge x) = 1 - F_X(x) = \int_x^\infty f_X(x) dx}
#' where X is the distribution, \eqn{S_X} is the survival function, \eqn{F_X} is the cdf and \eqn{f_X} is the pdf.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @return Survival function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
ExoticStatistics$set("public", "survival", function(x1, log = FALSE) {
  if(private$.isCdf)
    self$cdf(x1 = x1, lower.tail = FALSE, log.p = log)
  else {
    message(.distr6$message_numeric)
    surv = integrate(self$pdf, x1, self$sup())$value
    if(log)
      return(log(surv))
    else
      return(surv)
  }
})

#-------------------------------------------------------------
# Public Methods - hazard
#-------------------------------------------------------------
#' @title Hazard Function
#' @name hazard
#' @description The hazard function of a probability distribution is the risk of instantaneous event at
#' a point x.
#'
#' @usage hazard(object, x1, log = FALSE)
#' @section R6 Usage: $hazard(x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the hazard function is returned.
#'
#' @details The hazard function is defined analytically by
#' \deqn{h_X(x) = \frac{f_X}{S_X}}{h_X(x) = f_X/S_X}
#' where X is the distribution, \eqn{S_X} is the survival function and \eqn{f_X} is the pdf.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @return Hazard function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
ExoticStatistics$set("public", "hazard", function(x1, log = FALSE) {
  if(private$.isPdf)
    pdf = self$pdf(x1)
  else if(private$.isCdf){
    message(.distr6$message_numeric)
    pdf = try(as.numeric(attr(deriv(y~self$cdf(x1),"x1", func = TRUE)(x1),"gradient")),
            silent = TRUE)
    if(inherits(pdf,"try-error"))
      pdf = pracma::fderiv(self$cdf,x1)
  }

  surv = self$survival(x1)

  haz = pdf/surv

  if(log)
    return(log(haz))
  else
    return(haz)
})

#-------------------------------------------------------------
# Public Methods - cumHazard
#-------------------------------------------------------------
#' @title Cumulative Hazard Function
#' @name cumHazard
#' @description The cumulative hazard function of a probability distribution is the anti-derivative of
#' the hazard function.
#'
#' @usage cumHazard(object, x1, log = FALSE)
#' @section R6 Usage: $cumHazard(x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the cumulative hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the cumulative hazard function is returned.
#'
#' @details The cumulative hazard function is defined analytically by
#' \deqn{H_X(x) = -log(S_X)}
#' where X is the distribution and \eqn{S_X} is the survival function.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @return Cumulative hazard function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
ExoticStatistics$set("public", "cumHazard", function(x1, log = FALSE) {
  if(!log){
    return(-self$survival(x1, log = TRUE))
  } else
    return(log(-self$survival(x1, log = TRUE)))
})

#-------------------------------------------------------------
# Public Methods - cdfPNorm
#-------------------------------------------------------------
#' @title Cumulative Distribution Function P-Norm
#' @name cdfPNorm
#' @description The p-norm of the cdf evaluated between given limits or over the whole support.
#'
#' @usage cdfPNorm(object, p = 2, lower = NULL, upper = NULL)
#' @section R6 Usage: $cdfPNorm(object, p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the cdf is defined by
#' \deqn{(\int_a^b |F_X|^p d\mu)^{1/p}}
#' where X is the distribution, \eqn{F_X} is the cdf and \eqn{a, b} are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @return Given p-norm of cdf evaluated between limits as a numeric.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
ExoticStatistics$set("public", "cdfPNorm", function(p = 2, lower = NULL, upper = NULL) {
  if(is.null(lower)) lower <- self$inf()
  if(is.null(upper)) upper <- self$sup()

  if(testContinuous(self))
    return(generalPNorm(self$cdf, p, lower, upper))
})

#-------------------------------------------------------------
# Public Methods - pdfPNorm
#-------------------------------------------------------------
#' @title Probability Density Function P-Norm
#' @name pdfPNorm
#' @description The p-norm of the pdf evaluated between given limits or over the whole support.
#'
#' @usage pdfPNorm(object, p = 2, lower = NULL, upper = NULL)
#' @section R6 Usage: $pdfPNorm(p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the pdf is defined by
#' \deqn{(\int_a^b |f_X|^p d\mu)^{1/p}}
#' where X is the distribution, \eqn{f_X} is the pdf and a,b are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @return Given p-norm of pdf evaluated between limits as a numeric.
#'
#' @export
ExoticStatistics$set("public", "pdfPNorm", function(p = 2, lower = NULL, upper = NULL) {
  if(is.null(lower)) lower <- self$inf()
  if(is.null(upper)) upper <- self$sup()

  if(testContinuous(self))
    return(generalPNorm(self$pdf, p, lower, upper))
}) # NEEDS TESTING

#-------------------------------------------------------------
# squared2Norm
#-------------------------------------------------------------
#' @title Squared Probability Density Function 2-Norm
#' @name squared2Norm
#' @description The squared 2-norm of the pdf evaluated over the whole support by default or given
#' limits.
#'
#' @usage squared2Norm(object, lower = NULL, upper = NULL)
#' @section R6 Usage: $squared2Norm(lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The squared 2-norm of the pdf is defined by
#' \deqn{\int (f_X(u))^2 du}
#' where X is the Distribution and \eqn{f_X} is its pdf.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{ExoticStatistics}} decorator.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @return Squared 2-norm of pdf evaluated between limits as a numeric.
#'
#' @export
NULL
ExoticStatistics$set("public","squared2Norm",function(lower = NULL, upper = NULL){
  return(self$pdfPNorm(p = 2, lower = lower, upper = upper))
})

#-------------------------------------------------------------
# Public Methods - survivalPNorm
#-------------------------------------------------------------
#' @title Survival Function P-Norm
#' @name survivalPNorm
#' @description The p-norm of the survival function evaluated between given limits or over the whole support.
#'
#' @usage survivalPNorm(object, p = 2, lower = NULL, upper = NULL)
#' @section R6 Usage: $survivalPNorm(object, p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @details The p-norm of the survival function is defined by
#' \deqn{(\int_a^b |S_X|^p d\mu)^{1/p}}
#' where X is the distribution, \eqn{S_X} is the survival function and a,b are the limits of integration.
#'
#' Returns NULL if distribution is not continuous.
#'
#' Can only be used after decorating with \code{\link{ExoticStatistics}}.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @return Given p-norm of survival function evaluated between limits as a numeric.
#'
#' @export
ExoticStatistics$set("public", "survivalPNorm", function(p = 2, lower = NULL, upper = NULL){
  if(is.null(lower)) lower <- self$inf()
  if(is.null(upper)) upper <- self$sup()

  if(testContinuous(self))
    return(generalPNorm(self$survival, p, lower, upper))
})
