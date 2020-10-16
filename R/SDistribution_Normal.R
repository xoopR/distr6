# nolint start
#' @name Normal
#' @template SDist
#' @templateVar ClassName Normal
#' @templateVar DistName Normal
#' @templateVar uses in significance testing, for representing models with a bell curve, and as a result of the central limit theorem
#' @templateVar params variance, \eqn{\sigma^2}, and mean, \eqn{\mu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(x-\mu)^2/(2\sigma^2)) / \sqrt{2\pi\sigma^2}}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\sigma^2 > 0}
#' @templateVar distsupport the Reals
#' @templateVar aka Gaussian
#' @aliases Gaussian
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
Normal <- R6Class("Normal",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Normal",
    short_name = "Norm",
    description = "Normal Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `(numeric(1))`\cr
    #' Mean of the distribution, defined on the Reals.
    #' @param var `(numeric(1))`\cr
    #' Variance of the distribution, defined on the positive Reals.
    #' @param sd `(numeric(1))`\cr
    #' Standard deviation of the distribution, defined on the positive Reals. `sd = sqrt(var)`.
    #' If provided then `var` ignored.
    #' @param prec `(numeric(1))`\cr
    #' Precision of the distribution, defined on the positive Reals. `prec = 1/var`.
    #' If provided then `var` ignored.
    initialize = function(mean = 0, var = 1, sd = NULL, prec = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, var, sd, prec)
      self$setParameterValue(mean = mean, var = var, sd = sd, prec = prec)

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
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("var"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("var")))
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
        return(numeric(length(self$getParameterValue("var"))))
      } else {
        return(numeric(length(self$getParameterValue("var"))) + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      0.5 * log(2 * pi * exp(1) * unlist(self$getParameterValue("var")), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(exp((self$getParameterValue("mean") * t) +
                   (self$getParameterValue("var") * t^2 * 0.5)))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp((1i * self$getParameterValue("mean") * t) -
                   (self$getParameterValue("var") * t^2 * 0.5)))
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
      if (!is.null(lst$prec)) {
        lst$sd <- NULL
        lst$var <- NULL
      } else if (!is.null(lst$sd)) {
        lst$var <- NULL
      }
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "dnorm",
        x = x,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        log = log,
        vec = test_list(mean)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "pnorm",
        x = x,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(mean)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "qnorm",
        x = p,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(mean)
      )
    },
    .rand = function(n) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "rnorm",
        x = n,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        vec = test_list(mean)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table(
    ShortName = "Norm", ClassName = "Normal",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = "locscale"
  )
)
