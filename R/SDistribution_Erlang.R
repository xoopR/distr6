# nolint start
#' @name Erlang
#' @template SDist
#' @templateVar ClassName Erlang
#' @templateVar DistName Erlang
#' @templateVar uses as a special case of the Gamma distribution when the shape parameter is an integer
#' @templateVar params shape, \eqn{\alpha}, and rate, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)(x^{\alpha-1})(exp(-x\beta)) /(\alpha-1)!}
#' @templateVar paramsupport \eqn{\alpha = 1,2,3,\ldots} and \eqn{\beta > 0}
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
Erlang <- R6Class("Erlang",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Erlang",
    short_name = "Erlang",
    description = "Erlang Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param shape `(integer(1))`\cr
    #' Shape parameter, defined on the positive Naturals.
    initialize = function(shape = 1, rate = 1, scale = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet.Erlang(self, shape, rate, scale)
      self$setParameterValue(shape = shape, rate = rate, scale = scale)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("shape")) / unlist(self$getParameterValue("rate"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      (unlist(self$getParameterValue("shape")) - 1) / unlist(self$getParameterValue("rate"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("shape")) / (unlist(self$getParameterValue("rate"))^2)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      2 / sqrt(unlist(self$getParameterValue("shape")))
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
        return(6 / unlist(self$getParameterValue("shape")))
      } else {
        return((6 / unlist(self$getParameterValue("shape"))) + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      shape <- unlist(self$getParameterValue("shape"))
      rate <- unlist(self$getParameterValue("rate"))

      (1 - shape) * digamma(shape) + shape + log(gamma(shape / rate), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t < self$getParameterValue("rate")) {
        return((1 - self$getParameterValue("scale") * t)^(-self$getParameterValue("shape"))) # nolint
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      (1 - self$getParameterValue("scale") * 1i * t)^(-self$getParameterValue("shape")) # nolint
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
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        log = log,
        vec = test_list(shape)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "pgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qgamma",
        x = p,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .rand = function(n) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rgamma",
        x = n,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        vec = test_list(shape)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Erlang", ClassName = "Erlang",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
