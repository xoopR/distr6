#' @title Abstract Kernel Class
#'
#' @template class_abstract
#' @template field_package
#' @template field_packages
#' @template param_decorators
#' @template param_support
#' @template method_mode
#' @template method_pdfsquared2Norm
#'
#' @export
Kernel <- R6Class("Kernel",
  inherit = Distribution,
  lock_objects = FALSE,
  public = list(
    package = "This is now deprecated. Use $packages instead.",
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(decorators = NULL, support = Interval$new(-1, 1)) {
      abstract(self, "Kernel", "listKernels()")

      assert_pkgload(self$packages)

      if (!is.null(decorators)) suppressMessages(decorate(self, decorators))

      private$.properties$support <- assertSet(support)
      private$.traits$type <- Reals$new()
      private$.parameters <- ParameterSet$new()

      invisible(self)
    },

    #' @description
    #' Calculates the mode of the distribution.
    mode = function(which = "all") {
      return(0)
    },

    #' @description
    #' Calculates the mean (expectation) of the distribution.
    #' @param ... Unused.
    mean = function(...) {
      return(0)
    },

    #' @description
    #' Calculates the median of the distribution.
    median = function() {
      return(0)
    },

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {
      return(NULL)
    },

    #' @description
    #' The squared 2-norm of the cdf is defined by
    #' \deqn{\int_a^b (F_X(u))^2 du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    cdfSquared2Norm = function(x = 0, upper = Inf) {
      return(NULL)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) return(0)
  ),

  private = list(
    .isPdf = 1L,
    .isCdf = 1L,
    .isQuantile = 1L,
    .isRand = 1L,
    .log = TRUE,
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),
    .properties = list(kurtosis = NULL, skewness = 0, symmetric = "symmetric"),
    .rand = function(n) {
      if (!is.null(private$.quantile)) {
        return(self$quantile(runif(n)))
      } else {
        return(NULL)
      }
    }
  )
)

#' @title Squared Probability Density Function 2-Norm
#' @name pdfSquared2Norm
#' @description The squared 2-norm of the pdf evaluated up to a given limit, possibly shifted.
#'
#' @usage pdfSquared2Norm(object, x = 0, upper = Inf)
#'
#' @param object Distribution.
#' @param x amount to shift the result.
#' @param upper upper limit of the integral.
#'
#' @return Squared 2-norm of pdf evaluated between limits as a numeric.
#'
#' @export
NULL

#' @title Squared Cumulative Distribution Function 2-Norm
#' @name cdfSquared2Norm
#' @description The squared 2-norm of the cdf evaluated up to a given limit, possibly shifted.
#'
#' @usage cdfSquared2Norm(object, x = 0, upper = Inf)
#'
#' @param object Distribution.
#' @param x amount to shift the result.
#' @param upper upper limit of the integral.
#'
#' @return Squared 2-norm of cdf evaluated between limits as a numeric.
#'
#' @export
NULL
