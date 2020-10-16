# nolint start
#' @name StudentT
#' @author Chijing Zeng
#' @template SDist
#' @templateVar ClassName StudentT
#' @templateVar DistName Student's T
#' @templateVar uses to estimate the mean of populations with unknown variance from a small sample size, as well as in t-testing for difference of means and regression analysis
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \Gamma((\nu+1)/2)/(\sqrt(\nu\pi)\Gamma(\nu/2)) * (1+(x^2)/\nu)^(-(\nu+1)/2)}
#' @templateVar paramsupport \eqn{\nu > 0}
#' @templateVar distsupport the Reals
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
StudentT <- R6Class("StudentT",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "StudentT",
    short_name = "T",
    description = "Student's T Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df)
      self$setParameterValue(df = df)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      df <- unlist(self$getParameterValue("df"))
      mean <- rep(NaN, length(df))
      mean[df > 1] <- 0
      return(mean)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      numeric(length(self$getParameterValue("df")))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      df <- unlist(self$getParameterValue("df"))
      var <- rep(NaN, length(df))
      var[df > 2] <- df[df > 2] / (df[df > 2] - 2)
      return(var)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      df <- unlist(self$getParameterValue("df"))
      skew <- rep(NaN, length(df))
      skew[df > 3] <- 0
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
      exkurtosis <- rep(NaN, length(df))
      exkurtosis[df > 4] <- 6 / (df[df > 4] - 4)

      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
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
      return((((df + 1) / 2) * (digamma((1 + df) / 2) - digamma(df / 2))) +
        (log(sqrt(df) * beta(df / 2, 1 / 2), base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(NaN)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      df <- self$getParameterValue("df")
      return((besselK(sqrt(df) * abs(t), df / 2) * ((sqrt(df) * abs(t))^(df / 2))) / # nolint
               (gamma(df / 2) * 2^(df / 2 - 1))) # nolint
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "dt",
        x = x,
        args = list(df = unlist(df)),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "pt",
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
        fun = "qt",
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
        fun = "rt",
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
    ShortName = "T", ClassName = "StudentT",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
