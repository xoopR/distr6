# nolint start
#' @name ChiSquared
#' @template SDist
#' @templateVar ClassName ChiSquared
#' @templateVar DistName Chi-Squared
#' @templateVar uses to model the sum of independent squared Normal distributions and for confidence intervals
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\nu/2-1} exp(-x/2))/(2^{\nu/2}\Gamma(\nu/2))}
#' @templateVar paramsupport \eqn{\nu > 0}
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
#' @template param_df
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
ChiSquared <- R6Class("ChiSquared",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "ChiSquared",
    short_name = "ChiSq",
    description = "ChiSquared Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df)
      self$setParameterValue(df = df)

      if (df == 1) {
        support <- PosReals$new(zero = F)
      } else {
        support <- PosReals$new(zero = T)
      }

      super$initialize(
        decorators = decorators,
        support = support,
        type = PosReals$new(zero = TRUE)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("df"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      sapply(self$getParameterValue("df"), function(x) max(x - 2, 0))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("df")) * 2
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      sqrt(8 / unlist(self$getParameterValue("df")))
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
        return(12 / unlist(self$getParameterValue("df")))
      } else {
        return(12 / unlist(self$getParameterValue("df")) + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      df <- unlist(self$getParameterValue("df"))
      return(df / 2 + log(2 * gamma(df / 2), base) + ((1 - df / 2) * digamma(df / 2)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t < 0.5) {
        return((1 - 2 * t)^(-self$getParameterValue("df") / 2)) # nolint
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return((1 - 2i * t)^(-self$getParameterValue("df") / 2)) # nolint
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      if (z > 0 & z < sqrt(exp(1))) {
        return((1 - 2 * log(z))^(-self$getParameterValue("df") / 2)) # nolint
      } else {
        return(NaN)
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("df") == 1) {
        private$.properties$support <- PosReals$new(zero = F)
      } else {
        private$.properties$support <- PosReals$new(zero = T)
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "dchisq",
        x = x,
        args = list(df = unlist(df)),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "pchisq",
        x = x,
        args = list(df = unlist(df)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "qchisq",
        x = p,
        args = list(df = unlist(df)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .rand = function(n) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "rchisq",
        x = n,
        args = list(df = unlist(df)),
        vec = test_list(df)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "ChiSq", ClassName = "ChiSquared",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
