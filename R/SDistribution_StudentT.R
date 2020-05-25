
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
#'
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
StudentT <- R6Class("StudentT", inherit = SDistribution, lock_objects = F,
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
    mean = function() {
      df <- self$getParameterValue("df")
      if (df > 1) {
        return(0)
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = 'all') {
      return(0)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      df <- self$getParameterValue("df")
      if (df > 2) {
        return(df / (df - 2))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      if (self$getParameterValue("df") > 3) {
        return(0)
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      df <- self$getParameterValue("df")
      if (df > 4) {
        exkurtosis <- 6 / (df - 4)
      } else {
        return(NaN)
      }

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
    entropy = function(base = 2) {
      df <- self$getParameterValue("df")
      return((((df + 1) / 2) * (digamma((1 + df) / 2) - digamma(df / 2))) + (log(sqrt(df) * beta(df / 2, 1 / 2), base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      return(NaN)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      df <- self$getParameterValue("df")
      return((besselK(sqrt(df) * abs(t), df / 2) * ((sqrt(df) * abs(t))^(df / 2))) / (gamma(df / 2) * 2^(df / 2 - 1)))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
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

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$df)) lst <- c(lst, list(df = paramlst$df))
      return(lst)
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
    Package = "stats"
  )
)
