#' @title Exotic Statistical Methods Decorator
#'
#' @description This decorator adds methods for more complex statistical methods including p-norms,
#' survival and hazard functions and anti-derivatives. If possible analytical expressions are
#' exploited, otherwise numerical ones are used with a message.
#'
#' @template class_decorator
#' @template param_bounds
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#'
#' @examples
#' decorate(Exponential$new(), "ExoticStatistics")
#' Exponential$new(decorators = "ExoticStatistics")
#' ExoticStatistics$new()$decorate(Exponential$new())
#' @export
ExoticStatistics <- R6Class("ExoticStatistics",
  inherit = DistributionDecorator,
  public = list(

    #' @description
    #' The cdf anti-derivative is defined by \deqn{acdf(a, b) = \int_a^b F_X(x) dx}
    #' where X is the distribution, \eqn{F_X} is the cdf of the distribution \eqn{X} and
    #' \eqn{a, b} are the `lower` and `upper` limits of integration.
    cdfAntiDeriv = function(lower = NULL, upper = NULL) {
      self$cdfPNorm(1, lower, upper)
    },

    #' @description
    #' The survival anti-derivative is defined by
    #' \deqn{as(a, b) = \int_a^b S_X(x) dx}
    #' where X is the distribution, \eqn{S_X} is the survival function of the distribution
    #' \eqn{X} and \eqn{a, b} are the `lower` and `upper` limits of integration.
    survivalAntiDeriv = function(lower = NULL, upper = NULL) {
      self$survivalPNorm(1, lower, upper)
    },

    #' @description
    #' The survival function is defined by
    #' \deqn{S_X(x) = P(X \ge x) = 1 - F_X(x) = \int_x^\infty f_X(x) dx}
    #' where X is the distribution, \eqn{S_X} is the survival function, \eqn{F_X} is the cdf
    #' and \eqn{f_X} is the pdf.
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    survival = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      self$cdf(..., lower.tail = FALSE, log.p = log, simplify = simplify, data = data)
    },

    #' @description
    #' The hazard function is defined by
    #' \deqn{h_X(x) = \frac{f_X}{S_X}}{h_X(x) = f_X/S_X}
    #' where X is the distribution, \eqn{S_X} is the survival function and \eqn{f_X} is the pdf.
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
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
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
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
      if (testDiscrete(self)) {
        range <- as.numeric(self$workingSupport())
        if (!is.null(lower)) range <- range[range >= lower]
        if (!is.null(upper)) range <- range[range <= upper]
        return(generalPNorm(self$cdf, p, range = range))
      } else {
        if (is.null(lower)) lower <- self$inf
        if (is.null(upper)) upper <- self$sup
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
      if (testDiscrete(self)) {
        range <- as.numeric(self$workingSupport())
        if (!is.null(lower)) range <- range[range >= lower]
        if (!is.null(upper)) range <- range[range <= upper]
        return(generalPNorm(self$pdf, p, range = range))
      } else {
        if (is.null(lower)) lower <- self$inf
        if (is.null(upper)) upper <- self$sup
        return(generalPNorm(self$pdf, p, lower, upper))
      }
    },

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
      if (testDiscrete(self)) {
        range <- as.numeric(self$workingSupport())
        if (!is.null(lower)) range <- range[range >= lower]
        if (!is.null(upper)) range <- range[range <= upper]
        return(generalPNorm(self$survival, p, range = range))
      } else {
        if (is.null(lower)) lower <- self$inf
        if (is.null(upper)) upper <- self$sup
        return(generalPNorm(self$survival, p, lower, upper))
      }
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
#' @description See [ExoticStatistics]`$survival`.
#'
#' @usage survival(object, ..., log = FALSE, simplify = TRUE, data = NULL)
#'
#' @param object ([Distribution]).
#' @param ... `(numeric())` \cr
#' Points to evaluate the probability density function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param log `logical(1)` \cr If `TRUE` returns log-Hazard Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Survival function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @export
NULL

#' @title Hazard Function
#' @name hazard
#' @description See [ExoticStatistics]`$hazard`.
#'
#' @usage hazard(object, ..., log = FALSE, simplify = TRUE, data = NULL)
#'
#' @param object ([Distribution]).
#' @param ... `(numeric())` \cr
#' Points to evaluate the probability density function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param log `logical(1)` \cr If `TRUE` returns log-Hazard Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Hazard function as a numeric, natural logarithm returned if \code{log} is TRUE.
#'
#' @export
NULL

#' @title Cumulative Hazard Function
#' @name cumHazard
#' @description See [ExoticStatistics]`$cumHazard`.
#'
#' @usage cumHazard(object, ..., log = FALSE, simplify = TRUE, data = NULL)
#'
#' @param object ([Distribution]).
#' @param ... `(numeric())` \cr
#' Points to evaluate the probability density function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param log `logical(1)` \cr If `TRUE` returns log-cumHazard Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Cumulative hazard function as a numeric, natural logarithm returned if \code{log} is
#' TRUE.
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

#' @title Survival Function P-Norm
#' @name survivalPNorm
#' @description The p-norm of the survival function evaluated between given limits or over the
#' whole support.
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
