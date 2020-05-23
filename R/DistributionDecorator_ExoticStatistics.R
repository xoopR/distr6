#' @title Exotic Statistical Methods for Distributions
#'
#' @description This decorator adds methods for more complex statistical methods including p-norms,
#' survival and hazard functions and anti-derivatives. If possible analytical expressions are
#' exploited, otherwise numerical ones are used with a message.
#'
#' @template class_decorator
#' @template field_packages
#' @template param_bounds
#' @template method_cdf
#' @template method_pdf
#'
#' @examples
#' decorate(Exponential$new(), ExoticStatistics)
#' Exponential$new(decorators = ExoticStatistics)
#' ExoticStatistics$new()$decorate(Exponential$new())
#'
#' @export
ExoticStatistics <- R6Class("ExoticStatistics", inherit = DistributionDecorator,
  public = list(
    packages = "pracma",

    #' @description
    #' The cdf anti-derivative is defined by \deqn{acdf(a, b) = \int_a^b F_X(x) dx}
    #' where X is the distribution, \eqn{F_X} is the cdf of the distribution \eqn{X} and
    #' \eqn{a, b} are the `lower` and `upper` limits of integration.
    cdfAntiDeriv = function(lower = NULL, upper = NULL) {
      if (is.null(lower)) lower <- self$inf
      if (is.null(upper)) upper <- self$sup
      return(self$cdfPNorm(p = 1, lower, upper))
    },

    #' @description
    #' The survival anti-derivative is defined by
    #' \deqn{as(a, b) = \int_a^b S_X(x) dx}
    #' where X is the distribution, \eqn{S_X} is the survival function of the distribution
    #' \eqn{X} and \eqn{a, b} are the `lower` and `upper` limits of integration.
    survivalAntiDeriv = function(lower = NULL, upper = NULL) {
      if (is.null(lower)) lower <- self$inf
      if (is.null(upper)) upper <- self$sup
      return(self$survivalPNorm(p = 1, lower, upper))
    }, # NEEDS TESTING (p-norm)

    #' @description
    #' The survival function is defined by
    #' \deqn{S_X(x) = P(X \ge x) = 1 - F_X(x) = \int_x^\infty f_X(x) dx}
    #' where X is the distribution, \eqn{S_X} is the survival function, \eqn{F_X} is the cdf
    #' and \eqn{f_X} is the pdf.
    survival = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      self$cdf(..., lower.tail = FALSE, log.p = log, simplify = simplify, data = data)
    },

    #' @description
    #' The hazard function is defined by
    #' \deqn{h_X(x) = \frac{f_X}{S_X}}{h_X(x) = f_X/S_X}
    #' where X is the distribution, \eqn{S_X} is the survival function and \eqn{f_X} is the pdf.
    hazard = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      if (log) {
        pdf <- self$pdf(..., simplify = simplify, data = data, log = TRUE)
        surv <- self$survival(..., simplify = simplify, data = data, log = TRUE)
        return(pdf - surv)
      } else {
        pdf <- self$pdf(..., simplify = simplify, data = data, log = FALSE)
        surv <- self$survival(..., simplify = simplify, data = data, log = FALSE)
        return(pdf / surv)
      }
    },

    #' @description
    #' The cumulative hazard function is defined analytically by
    #' \deqn{H_X(x) = -log(S_X)}
    #' where X is the distribution and \eqn{S_X} is the survival function.
    cumHazard = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      if (!log) {
        return(-self$survival(..., log = TRUE, simplify = simplify, data = data))
      } else {
        return(log(-self$survival(..., log = TRUE, simplify = simplify, data = data)))
      }
    },

    #' @description
    #' The p-norm of the cdf is defined by
    #' \deqn{(\int_a^b |F_X|^p d\mu)^{1/p}}
    #' where X is the distribution, \eqn{F_X} is the cdf and \eqn{a, b}
    #' are the `lower` and `upper` limits of integration.
    #'
    #' Returns NULL if distribution is not continuous.
    #'
    #' @param p `(integer(1))`
    #' Norm to evaluate.
    cdfPNorm = function(p = 2, lower = NULL, upper = NULL) {
      if (is.null(lower)) lower <- self$inf
      if (is.null(upper)) upper <- self$sup

      if (testContinuous(self)) {
        return(generalPNorm(self$cdf, p, lower, upper))
      }
    },

    #' @description
    #' The p-norm of the pdf is defined by
    #' \deqn{(\int_a^b |f_X|^p d\mu)^{1/p}}
    #' where X is the distribution, \eqn{f_X} is the pdf and \eqn{a, b}
    #' are the `lower` and `upper` limits of integration.
    #'
    #' Returns NULL if distribution is not continuous.
    #'
    #' @param p `(integer(1))`
    #' Norm to evaluate.
    pdfPNorm = function(p = 2, lower = NULL, upper = NULL) {
      if (is.null(lower)) lower <- self$inf
      if (is.null(upper)) upper <- self$sup

      if (testContinuous(self)) {
        return(generalPNorm(self$pdf, p, lower, upper))
      }
    }, # NEEDS TESTING

    #' @description
    #' The p-norm of the survival function is defined by
    #' \deqn{(\int_a^b |S_X|^p d\mu)^{1/p}}
    #' where X is the distribution, \eqn{S_X} is the survival function and \eqn{a, b}
    #' are the `lower` and `upper` limits of integration.
    #'
    #' Returns NULL if distribution is not continuous.
    #'
    #' @param p `(integer(1))`
    #' Norm to evaluate.
    survivalPNorm = function(p = 2, lower = NULL, upper = NULL) {
      if (is.null(lower)) lower <- self$inf
      if (is.null(upper)) upper <- self$sup

      if (testContinuous(self)) {
        return(generalPNorm(self$survival, p, lower, upper))
      }
    },

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the `lower` and `upper` limits of integration.
    #'
    #' Returns NULL if distribution is not continuous.
    squared2Norm = function(lower = NULL, upper = NULL) {
      return(self$pdfPNorm(p = 2, lower = lower, upper = upper)^2)
    }
  )
)

.distr6$decorators <- append(.distr6$decorators, list(ExoticStatistics = ExoticStatistics))

#' @title Cumulative Distribution Function Anti-Derivative
#' @name cdfAntiDeriv
#' @description The anti-derivative of the cumulative distribution function between given limits or
#' over the full support.
#'
#' @usage cdfAntiDeriv(object, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @return Antiderivative of the cdf evaluated between limits as a numeric.
#'
#' @export
NULL

#' @title Survival Function Anti-Derivative
#' @name survivalAntiDeriv
#' @description The anti-derivative of the survival function between given limits or
#' over the full support.
#'
#' @usage survivalAntiDeriv(object, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @return Antiderivative of the survival function evaluated between limits as a numeric.
#'
#' @export
NULL

#' @title Survival Function
#' @name survival
#' @description The survival function of a probability distribution is the probability of surviving
#' after a point x.
#'
#' @usage survival(object, x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the survival function at.
#' @param log logical, if TRUE then the (natural) logarithm of the survival function is returned.
#'
#' @return Survival function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @export
NULL

#' @title Hazard Function
#' @name hazard
#' @description The hazard function of a probability distribution is the risk of instantaneous event at
#' a point x.
#'
#' @usage hazard(object, x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the hazard function is returned.
#'
#' @return Hazard function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @export
NULL

#' @title Cumulative Hazard Function
#' @name cumHazard
#' @description The cumulative hazard function of a probability distribution is the anti-derivative of
#' the hazard function.
#'
#' @usage cumHazard(object, x1, log = FALSE)
#'
#' @param object Distribution.
#' @param x1 Point to evaluate the cumulative hazard function at.
#' @param log logical, if TRUE then the (natural) logarithm of the cumulative hazard function is returned.
#'
#' @return Cumulative hazard function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @export
NULL

#' @title Cumulative Distribution Function P-Norm
#' @name cdfPNorm
#' @description The p-norm of the cdf evaluated between given limits or over the whole support.
#'
#' @usage cdfPNorm(object, p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @return Given p-norm of cdf evaluated between limits as a numeric.
#'
#' @export
NULL

#' @title Probability Density Function P-Norm
#' @name pdfPNorm
#' @description The p-norm of the pdf evaluated between given limits or over the whole support.
#'
#' @usage pdfPNorm(object, p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @seealso \code{\link{ExoticStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL

#' @title Squared Probability Density Function 2-Norm
#' @name squared2Norm
#' @description The squared 2-norm of the pdf evaluated over the whole support by default or given
#' limits.
#'
#' @usage squared2Norm(object, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @return Squared 2-norm of pdf evaluated between limits as a numeric.
#'
#' @export
NULL

#' @title Survival Function P-Norm
#' @name survivalPNorm
#' @description The p-norm of the survival function evaluated between given limits or over the whole support.
#'
#' @usage survivalPNorm(object, p = 2, lower = NULL, upper = NULL)
#'
#' @param object Distribution.
#' @param p p-norm to calculate.
#' @param lower lower limit for integration, default is infimum.
#' @param upper upper limit for integration, default is supremum.
#'
#' @return Given p-norm of survival function evaluated between limits as a numeric.
#'
#' @export
NULL
