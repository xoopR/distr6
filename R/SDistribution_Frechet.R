# nolint start
#' @name Frechet
#' @template SDist
#' @templateVar ClassName Frechet
#' @templateVar DistName Frechet
#' @templateVar uses as a special case of the Generalised Extreme Value distribution
#' @templateVar params shape, \eqn{\alpha}, scale, \eqn{\beta}, and minimum, \eqn{\gamma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)((x-\gamma)/\beta)^{-1-\alpha}exp(-(x-\gamma)/\beta)^{-\alpha}}
#' @templateVar paramsupport \eqn{\alpha, \beta \epsilon R^+} and \eqn{\gamma \epsilon R}
#' @templateVar distsupport \eqn{x > \gamma}
#' @templateVar aka Inverse Weibull
#' @aliases InverseWeibull
# nolint end
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
Frechet <- R6Class("Frechet",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Frechet",
    short_name = "Frec",
    description = "Frechet Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param minimum `(numeric(1))`\cr
    #' Minimum of the distribution, defined on the Reals.
    initialize = function(shape = 1, scale = 1, minimum = 0,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, shape, scale, minimum)
      self$setParameterValue(shape = shape, scale = scale, minimum = minimum)

      super$initialize(
        decorators = decorators,
        support = Interval$new(minimum, Inf, type = "()"),
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
      shape <- unlist(self$getParameterValue("shape"))
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))
      mean <- rep(Inf, length(shape))
      mean[shape > 1] <- minimum[shape > 1] + scale[shape > 1] * gamma(1 - 1 / shape[shape > 1])
      return(mean)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      shape <- unlist(self$getParameterValue("shape"))
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))

      return(minimum + scale * (shape / (1 + shape))^(1 / shape)) # nolint
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      m <- unlist(self$getParameterValue("minimum"))
      s <- unlist(self$getParameterValue("scale"))
      a <- unlist(self$getParameterValue("shape"))

      return(m + s / (log(2)^(1 / a))) # nolint
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      shape <- unlist(self$getParameterValue("shape"))
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))
      var <- rep(Inf, length(shape))
      var[shape > 2] <- scale[shape > 2]^2 * (gamma(1 - 2 / shape[shape > 2]) -
        gamma(1 - 1 / shape[shape > 2])^2)
      return(var)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      shape <- unlist(self$getParameterValue("shape"))
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))
      skew <- rep(Inf, length(shape))
      skew[shape > 3] <- (gamma(1 - 3 / shape[shape > 3]) - 3 *
        gamma(1 - 2 / shape[shape > 3]) * gamma(1 - 1 / shape[shape > 3]) + 2
      * gamma(1 - 1 / shape[shape > 3])^3) /
        ((gamma(1 - 2 / shape[shape > 3]) - gamma(1 - 1 / shape[shape > 3])^2)^(3 / 2)) # nolint
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
      shape <- unlist(self$getParameterValue("shape"))
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))
      kur <- rep(Inf, length(shape))
      kur[shape > 4] <- (gamma(1 - 4 / shape[shape > 4]) - 4 * gamma(1 - 3 / shape[shape > 4]) *
        gamma(1 - 1 / shape[shape > 4]) + 3 * gamma(1 - 2 / shape[shape > 4])^2) /
        ((gamma(1 - 2 / shape[shape > 4]) - gamma(1 - 1 / shape[shape > 4])^2)^2)
      if (excess) {
        return(kur - 6)
      } else {
        return(kur - 3)
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
      minimum <- unlist(self$getParameterValue("minimum"))
      scale <- unlist(self$getParameterValue("scale"))

      return(1 - digamma(1) / shape - digamma(1) + log(scale / shape, base))
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
      private$.properties$support <- Interval$new(self$getParameterValue("minimum"),
                                                  Inf, type = "()")
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::dfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dfrechet(
          x,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::pfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pfrechet(
          x,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qfrechet(
          p,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rfrechet(
          n,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Frec", ClassName = "Frechet",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "locscale"
  )
)
