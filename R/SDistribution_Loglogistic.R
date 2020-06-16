
#' @name Loglogistic
#' @template SDist
#' @templateVar ClassName Loglogistic
#' @templateVar DistName Log-Logistic
#' @templateVar uses in survival analysis for its non-monotonic hazard as well as in economics
#' @templateVar params shape, \eqn{\beta}, and scale, \eqn{\alpha}
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta/\alpha)(x/\alpha)^{\beta-1}(1 + (x/\alpha)^\beta)^{-2}}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the non-negative Reals
#' @templateVar aka Fisk
#' @aliases Fisk
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_scale
#' @template param_shape
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Loglogistic <- R6Class("Loglogistic",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Loglogistic",
    short_name = "LLogis",
    description = "Loglogistic Probability Distribution.",
    packages = "actuar",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param rate `(numeric(1))`\cr
    #' Alternate scale parameter, `rate = 1/scale`. If given then `scale` is ignored.
    initialize = function(scale = 1, shape = 1, rate = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, scale, shape, rate)
      self$setParameterValue(scale = scale, shape = shape, rate = rate)

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
    mean = function() {
      return((self$getParameterValue("scale") * pi / self$getParameterValue("shape")) /
        sin(pi / self$getParameterValue("shape")))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      shape <- self$getParameterValue("shape")
      return(self$getParameterValue("scale") * ((shape - 1) / (shape + 1))^(1 / shape))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      return(self$getParameterValue("scale"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      if (self$getParameterValue("shape") > 2) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        return(scale^2 * ((2 * shapi) / sin(2 * shapi) - (shapi^2) / sin(shapi)^2))
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
      if (self$getParameterValue("shape") > 3) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        s1 <- (2 * shapi^3 * scale^3) / sin(shapi)^3
        s2 <- (6 * shapi^2 * scale^3) * (1 / sin(shapi)) * (1 / sin(2 * shapi))
        s3 <- (3 * shapi * scale^3) / sin(3 * shapi)
        return(s1 - s2 + s3)
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
      if (self$getParameterValue("shape") > 4) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        s1 <- (3 * shapi^4 * scale^4) / sin(shapi)^4
        s2 <- (12 * shapi^3 * scale^4) * (1 / sin(shapi)^2) * (1 / sin(2 * shapi))
        s3 <- (12 * shapi^2 * scale^4) * (1 / sin(shapi)) * (1 / sin(3 * shapi))
        s4 <- (4 * shapi * scale^4) * (1 / sin(4 * shapi))
        kurtosis <- -s1 + s2 - s3 + s4
        if (excess) {
          return(kurtosis - 3)
        } else {
          return(kurtosis)
        }
      } else {
        return(NaN)
      }
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
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::dllogis,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        actuar::dllogis(x, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"), log = log)
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::pllogis,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        actuar::pllogis(x,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::qllogis,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        actuar::qllogis(p,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::rllogis,
          shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
          MoreArgs = list(n = n)
        )
      } else {
        actuar::rllogis(n, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"))
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "LLogis", ClassName = "Loglogistic",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "actuar"
  )
)
