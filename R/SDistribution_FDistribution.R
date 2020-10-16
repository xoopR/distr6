# nolint start
#' @name FDistribution
#' @template SDist
#' @templateVar ClassName FDistribution
#' @templateVar DistName 'F'
#' @templateVar uses in ANOVA testing and is the ratio of scaled Chi-Squared distributions.
#' @templateVar params two degrees of freedom parameters, \eqn{\mu, \nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \Gamma((\mu + \nu)/2) / (\Gamma(\mu/2) \Gamma(\nu/2)) (\mu/\nu)^{\mu/2} x^{\mu/2 - 1} (1 + (\mu/\nu) x)^{-(\mu + \nu)/2}}
#' @templateVar paramsupport \eqn{\mu, \nu > 0}
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
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
FDistribution <- R6Class("FDistribution",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "FDistribution",
    short_name = "F",
    description = "F Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param df1 `(numeric(1))`\cr
    #' First degree of freedom of the distribution defined on the positive Reals.
    #' @param df2 `(numeric(1))`\cr
    #' Second degree of freedom of the distribution defined on the positive Reals.
    initialize = function(df1 = 1, df2 = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df1, df2)
      self$setParameterValue(df1 = df1, df2 = df2)

      if (df1 == 1) {
        support <- PosReals$new(zero = FALSE)
      } else {
        support <- PosReals$new(zero = TRUE)
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
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      mean <- rep(NaN, length(df1))
      mean[df2 > 2] <- df2[df2 > 2] / (df2[df2 > 2] - 2)
      return(mean)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      mode <- rep(NaN, length(df1))
      mode[df1 > 2] <- ((df1[df1 > 2] - 2) * df2[df1 > 2]) / (df1[df1 > 2] * (df2[df1 > 2] + 2))
      return(mode)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      var <- rep(NaN, length(df1))
      var[df2 > 4] <- 2 * (df2[df2 > 4])^2 * (df1[df2 > 4] + df2[df2 > 4] - 2) /
        (df1[df2 > 4] * (df2[df2 > 4] - 2)^2 * (df2[df2 > 4] - 4))
      return(var)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      skew <- rep(NaN, length(df1))
      skew[df2 > 6] <- ((2 * df1[df2 > 6] + df2[df2 > 6] - 2) * sqrt(8 * (df2[df2 > 6] - 4))) /
        (((df2[df2 > 6] - 6) * sqrt(df1[df2 > 6] * (df1[df2 > 6] + df2[df2 > 6] - 2))))
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
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      exkurtosis <- rep(NaN, length(df1))
      exkurtosis[df2 > 8] <- (12 * (df1[df2 > 8] * (5 * df2[df2 > 8] - 22) *
        (df1[df2 > 8] + df2[df2 > 8] - 2) +
        (df2[df2 > 8] - 4) * (df2[df2 > 8] - 2)^2)) /
        (df1[df2 > 8] * (df2[df2 > 8] - 6) * (df2[df2 > 8] - 8) * (df1[df2 > 8] + df2[df2 > 8] - 2))

      if (excess == TRUE) {
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
      df1 <- unlist(self$getParameterValue("df1"))
      df2 <- unlist(self$getParameterValue("df2"))
      return(log(gamma(df1 / 2), base) + log(gamma(df2 / 2), base) -
        log(gamma((df1 + df2) / 2), base) + log(df1 / df2, base) + (1 - df1 / 2)
        * digamma(1 + df1 / 2) - (1 + df2 / 2) * digamma(1 + df2 / 2) +
        ((df1 + df2) / 2) * digamma((df1 + df2) / 2))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(NaN)
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
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("df1") == 1) {
        private$.properties$support <- PosReals$new(zero = FALSE)
      } else {
        private$.properties$support <- PosReals$new(zero = TRUE)
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      call_C_base_pdqr(
        fun = "df",
        x = x,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2)
        ),
        log = log,
        vec = test_list(df1)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      call_C_base_pdqr(
        fun = "pf",
        x = x,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df1)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      call_C_base_pdqr(
        fun = "qf",
        x = p,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df1)
      )
    },
    .rand = function(n) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      call_C_base_pdqr(
        fun = "rf",
        x = n,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2)
        ),
        vec = test_list(df1)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "F", ClassName = "FDistribution",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
