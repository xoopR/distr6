# nolint start
#' @name Logarithmic
#' @template SDist
#' @templateVar ClassName Logarithmic
#' @templateVar DistName Logarithmic
#' @templateVar uses to model consumer purchase habits in economics and is derived from the Maclaurin series expansion of \eqn{-ln(1-p)}
#' @templateVar params a parameter, \eqn{\theta},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = -\theta^x/xlog(1-\theta)}
#' @templateVar paramsupport \eqn{0 < \theta < 1}
#' @templateVar distsupport \eqn{{1,2,3,\ldots}}
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
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
Logarithmic <- R6Class("Logarithmic",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Logarithmic",
    short_name = "Log",
    description = "Logarithmic Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param theta `(numeric(1))`\cr
    #' Theta parameter defined as a probability between `0` and `1`.
    initialize = function(theta = 0.5, decorators = NULL) {

      private$.parameters <- getParameterSet.Logarithmic(self, theta)
      self$setParameterValue(theta = theta)

      super$initialize(
        decorators = decorators,
        support = PosNaturals$new(),
        type = PosNaturals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      theta <- unlist(self$getParameterValue("theta"))
      return(-theta / (log(1 - theta) * (1 - theta)))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      rep(1, length(self$getParameterValue("theta")))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      theta <- unlist(self$getParameterValue("theta"))
      return((-theta^2 - theta * log(1 - theta)) / ((1 - theta)^2 * (log(1 - theta))^2))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      theta <- unlist(self$getParameterValue("theta"))

      s1 <- (theta * (3 * theta + theta * log(1 - theta) + log(1 - theta))) /
        ((theta - 1)^3 * log(1 - theta)^2)
      s2 <- 2 * (-theta / (log(1 - theta) * (1 - theta)))^3

      return((s1 + s2) / (self$stdev()^3))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      theta <- unlist(self$getParameterValue("theta"))

      s1 <- (3 * theta^4) / ((1 - theta)^4 * log(1 - theta)^4)
      s2 <- (6 * theta^3) / ((theta - 1)^4 * log(1 - theta)^3)
      s3 <- (4 * theta^3) / ((theta - 1)^4 * log(1 - theta)^2)
      s4 <- (theta^3) / ((theta - 1)^4 * log(1 - theta))
      s5 <- (4 * theta^2) / ((theta - 1)^4 * log(1 - theta)^2)
      s6 <- (4 * theta^2) / ((theta - 1)^4 * log(1 - theta))
      s7 <- (theta) / ((theta - 1)^4 * log(1 - theta))

      sum <- -s1 - s2 - s3 - s4 - s5 - s6 - s7

      kurtosis <- sum / (self$stdev()^4)

      if (excess) {
        return(kurtosis - 3)
      } else {
        return(kurtosis)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t < -log(self$getParameterValue("theta"))) {
        return(log(1 - self$getParameterValue("theta") * exp(t)) /
                 log(1 - self$getParameterValue("theta")))
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(log(1 - self$getParameterValue("theta") * exp(t * 1i)) /
               log(1 - self$getParameterValue("theta")))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      if (abs(z) < 1 / self$getParameterValue("theta")) {
        return(log(1 - self$getParameterValue("theta") * z) /
                 log(1 - self$getParameterValue("theta")))
      } else {
        return(NaN)
      }
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("theta"))) {
        mapply(extraDistr::dlgser,
          theta = self$getParameterValue("theta"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dlgser(x, theta = self$getParameterValue("theta"), log = log)
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("theta"))) {
        mapply(extraDistr::plgser,
          theta = self$getParameterValue("theta"),
          MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::plgser(x,
          theta = self$getParameterValue("theta"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("theta"))) {
        mapply(extraDistr::qlgser,
          theta = self$getParameterValue("theta"),
          MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::qlgser(p,
          theta = self$getParameterValue("theta"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("theta"))) {
        mapply(extraDistr::rlgser,
          theta = self$getParameterValue("theta"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rlgser(n, theta = self$getParameterValue("theta"))
      }
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Log", ClassName = "Logarithmic",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = ""
  )
)
