
#' @name Weibull
#' @template SDist
#' @templateVar ClassName Weibull
#' @templateVar DistName Weibull
#' @templateVar uses in survival analysis as it satisfies both PH and AFT requirements
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)(x/\beta)^{\alpha-1}exp(-x/\beta)^\alpha}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Positive Reals
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_shape
#' @template param_scale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Weibull <- R6Class("Weibull",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Weibull",
    short_name = "Weibull",
    description = "Weibull Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param altscale `(numeric(1))`\cr
    #' Alternative scale parameter, if given then `scale` is ignored.
    #' `altscale = scale^-shape`.
    initialize = function(shape = 1, scale = 1, altscale = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, shape, scale, altscale)
      self$setParameterValue(shape = shape, scale = scale, altscale = altscale)

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
      unlist(self$getParameterValue("scale")) *
        gamma(1 + 1 / unlist(self$getParameterValue("shape")))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      mode <- numeric(length(scale))
      mode[shape > 1] <- scale[shape > 1] *
        ((shape[shape > 1] - 1) / shape[shape > 1])^(1 / shape[shape > 1]) # nolint
      return(mode)
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      unlist(self$getParameterValue("scale")) *
        (log(2)^(1 / unlist(self$getParameterValue("shape")))) # nolint
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      return(scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      mu <- self$mean()
      sigma <- self$stdev()
      return(((gamma(1 + 3 / shape) * (scale^3)) - (3 * mu * sigma^2) - (mu^3)) / (sigma^3))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      skew <- self$skewness()
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      mu <- self$mean()
      sigma <- self$stdev()

      kur <- (((scale^4) * gamma(1 + 4 / shape)) - (4 * skew * (sigma^3) * mu) -
        (6 * (sigma^2) * (mu^2)) - (mu^4)) / (sigma^4)

      if (excess) {
        return(kur - 3)
      } else {
        return(kur)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      return(-digamma(1) * (1 - 1 / shape) + log(scale / shape, base) + 1)
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
      if (!is.null(lst$altscale)) lst$scale <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "dweibull",
        x = x,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        log = log,
        vec = test_list(shape)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "pweibull",
        x = x,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "qweibull",
        x = p,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .rand = function(n) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "rweibull",
        x = n,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
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
    ShortName = "Weibull", ClassName = "Weibull",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
