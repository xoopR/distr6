#' @title Exotic Statistical Methods Decorator
#'
#' @description This decorator adds methods for more complex statistical methods including p-norms,
#' survival and hazard functions and anti-derivatives. If possible analytical expressions are
#' exploited, otherwise numerical ones are used with a message.
#'
#' @details Numerical approximations will not work for multivariate distributions.
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
