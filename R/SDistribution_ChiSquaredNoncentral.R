# nolint start
#' @name ChiSquaredNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName ChiSquaredNoncentral
#' @templateVar DistName Noncentral Chi-Squared
#' @templateVar uses to model the sum of independent squared Normal distributions and for confidence intervals
#' @templateVar params degrees of freedom, \eqn{\nu}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-\lambda/2) \sum_{r=0}^\infty ((\lambda/2)^r/r!) (x^{(\nu+2r)/2-1}exp(-x/2))/(2^{(\nu+2r)/2}\Gamma((\nu+2r)/2))}
#' @templateVar paramsupport \eqn{\nu \ge 0}, \eqn{\lambda \ge 0}
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
#' @template param_poslocation
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
ChiSquaredNoncentral <- R6Class("ChiSquaredNoncentral",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "ChiSquaredNoncentral",
    short_name = "ChiSqNC",
    description = "Non-central ChiSquared Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, location = 0, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df, location)
      self$setParameterValue(df = df, location = location)

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
      unlist(self$getParameterValue("df")) + unlist(self$getParameterValue("location"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      2 * (unlist(self$getParameterValue("df")) + 2 * unlist(self$getParameterValue("location")))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      df <- unlist(self$getParameterValue("df"))
      ncp <- unlist(self$getParameterValue("location"))
      skew <- rep(NaN, length(df))
      skew[df + ncp != 0] <- ((2^(3 / 2)) * (df + 3 * ncp)) / ((df + 2 * ncp)^(3 / 2)) # nolint
      return(skew)
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      df <- unlist(self$getParameterValue("df"))
      ncp <- unlist(self$getParameterValue("location"))

      kur <- rep(NaN, length(df))
      kur[df + ncp != 0] <- (12 * (df + 4 * ncp)) / ((df + 2 * ncp)^2)

      if (excess) {
        return(kur)
      } else {
        return(kur + 3)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t < 0.5) {
        return(exp(self$getParameterValue("location") * t / (1 - 2 * t)) / ((1 - 2 * t)^(self$getParameterValue("df") / 2))) # nolint
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp(self$getParameterValue("location") * 1i * t / (1 - 2i * t)) / ((1 - 2i * t)^(self$getParameterValue("df") / 2))) # nolint
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("df") <= 1) {
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
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "dchisq",
        x = x,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "pchisq",
        x = x,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "qchisq",
        x = p,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .rand = function(n) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "rchisq",
        x = n,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
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
    ShortName = "ChiSqNC", ClassName = "ChiSquaredNoncentral",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
