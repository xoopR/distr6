
#' @name Wald
#' @template SDist
#' @templateVar ClassName Wald
#' @templateVar DistName Wald
#' @templateVar uses for modelling the first passage time for Brownian motion
#' @templateVar params mean, \eqn{\mu}, and shape, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\lambda/(2x^3\pi))^{1/2} exp((-\lambda(x-\mu)^2)/(2\mu^2x))}
#' @templateVar paramsupport \eqn{\lambda > 0} and \eqn{\mu > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedDPQR \code{quantile}
#' @templateVar aka Inverse Normal
#' @aliases InverseNormal InverseGaussian
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @details
#' Sampling is performed as per Michael, Schucany, Haas (1976).
#'
#' @references
#' Michael, J. R., Schucany, W. R., & Haas, R. W. (1976).
#' Generating random variates using transformations with multiple roots.
#' The American Statistician, 30(2), 88-90.
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
#' @template field_packages
#'
#' @export
Wald <- R6Class("Wald",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Wald",
    short_name = "Wald",
    description = "Wald Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `(numeric(1))`\cr
    #' Mean of the distribution, location parameter, defined on the positive Reals.
    initialize = function(mean = 1, shape = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, shape)
      self$setParameterValue(mean = mean, shape = shape)

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
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      mean <- unlist(self$getParameterValue("mean"))
      shape <- unlist(self$getParameterValue("shape"))
      return(mean * ((1 + (9 * mean^2) / (4 * shape^2))^0.5 - (3 * mean) / (2 * shape)))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("mean"))^3 / unlist(self$getParameterValue("shape"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      3 * (unlist(self$getParameterValue("mean")) / unlist(self$getParameterValue("shape")))^0.5
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
        return(15 * unlist(self$getParameterValue("mean")) /
          unlist(self$getParameterValue("shape")))
      } else {
        return(15 * unlist(self$getParameterValue("mean")) /
          unlist(self$getParameterValue("shape")) + 3)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      mean <- self$getParameterValue("mean")
      shape <- self$getParameterValue("shape")
      return(exp(shape / mean * (1 - sqrt(1 - 2 * mean^2 * t / shape))))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      mean <- self$getParameterValue("mean")
      shape <- self$getParameterValue("shape")
      return(exp(shape / mean * (1 - sqrt(1 - 2 * mean^2 * 1i * t / shape))))
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
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::dwald,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dwald(x,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(
          extraDistr::pwald,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::pwald(x,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::rwald,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rwald(n,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),

    .isQuantile = FALSE
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Wald", ClassName = "Wald",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = ""
  )
)
