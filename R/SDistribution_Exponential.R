# nolint start
#' @name Exponential
#' @template SDist
#' @templateVar ClassName Exponential
#' @templateVar DistName Exponential
#' @templateVar uses to model inter-arrival times in a Poisson process and has the memoryless property
#' @templateVar params rate, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \lambda exp(-x\lambda)}
#' @templateVar paramsupport \eqn{\lambda > 0}
#' @templateVar distsupport the Positive Reals
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_ratescale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Exponential <- R6Class("Exponential",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Exponential",
    short_name = "Exp",
    description = "Exponential Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(rate = 1, scale = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, rate, scale)
      self$setParameterValue(rate = rate, scale = scale)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new(zero = T)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("scale"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      numeric(length(self$getParameterValue("scale")))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      unlist(self$getParameterValue("scale")) * log(2)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("scale"))^2
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      rep(2, length(self$getParameterValue("scale")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      if (excess) {
        rep(6, length(self$getParameterValue("scale")))
      } else {
        rep(9, length(self$getParameterValue("scale")))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      1 - log(unlist(self$getParameterValue("rate")), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t < self$getParameterValue("rate")) {
        return(self$getParameterValue("rate") / (self$getParameterValue("rate") - t))
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(self$getParameterValue("rate") / (self$getParameterValue("rate") - ((0 + 1i) * t)))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$scale)) lst$rate <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dexp",
        x = x,
        args = list(rate = unlist(rate)),
        log = log,
        vec = test_list(rate)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "pexp",
        x = x,
        args = list(rate = unlist(rate)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(rate)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qexp",
        x = p,
        args = list(rate = unlist(rate)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(rate)
      )
    },
    .rand = function(n) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rexp",
        x = n,
        args = list(rate = unlist(rate)),
        vec = test_list(rate)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Exp", ClassName = "Exponential",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = "scale"
  )
)
