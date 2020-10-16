# nolint start
#' @name InverseGamma
#' @template SDist
#' @templateVar ClassName InverseGamma
#' @templateVar DistName Inverse Gamma
#' @templateVar uses in Bayesian statistics as the posterior distribution from the unknown variance in a Normal distribution
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha)x^{-\alpha-1}exp(-\beta/x)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{\Gamma} is the gamma function
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
#' @template param_shape
#' @template param_scale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
InverseGamma <- R6Class("InverseGamma",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "InverseGamma",
    short_name = "InvGamma",
    description = "Inverse Gamma Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, scale = 1, decorators = NULL) {

      private$.parameters <- getParameterSet.InverseGamma(self, shape, scale)
      self$setParameterValue(shape = shape, scale = scale)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(),
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
      shape <- unlist(self$getParameterValue("shape"))
      scale <- unlist(self$getParameterValue("scale"))
      mean <- rep(NaN, length(shape))
      mean[shape > 1] <- scale[shape > 1] / (shape[shape > 1] - 1)
      return(mean)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("scale")) / (unlist(self$getParameterValue("shape")) + 1)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      shape <- unlist(self$getParameterValue("shape"))
      scale <- unlist(self$getParameterValue("scale"))
      var <- rep(NaN, length(shape))
      var[shape > 2] <- scale[shape > 2]^2 / ((shape[shape > 2] - 1)^2 * (shape[shape > 2] - 2))
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
      skew <- rep(NaN, length(shape))
      skew[shape > 3] <- (4 * sqrt(shape[shape > 3] - 2)) / (shape[shape > 3] - 3)
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
      kur <- rep(NaN, length(shape))
      kur[shape > 4] <- (6 * (5 * shape[shape > 4] - 11)) /
        ((shape[shape > 4] - 3) * (shape[shape > 4] - 4))
      if (excess) {
        return(kur)
      } else {
        return(kur + 3)
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
      scale <- unlist(self$getParameterValue("scale"))

      return(shape + log(scale * gamma(shape), base) - (1 + shape) * digamma(shape))
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
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::dinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dinvgamma(
          x,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::pinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pinvgamma(
          x,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qinvgamma(
          p,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rinvgamma(
          n,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale")
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
    ShortName = "InvGamma", ClassName = "InverseGamma",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = ""
  )
)
