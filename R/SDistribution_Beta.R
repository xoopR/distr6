
#' @name Beta
#' @template SDist
#' @templateVar ClassName Beta
#' @templateVar DistName Beta
#' @templateVar uses as the prior in Bayesian modelling
#' @templateVar params two shape parameters, \eqn{\alpha, \beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\alpha-1}(1-x)^{\beta-1}) / B(\alpha, \beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{B} is the Beta function
#' @templateVar distsupport \eqn{[0, 1]}
#'
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
Beta <- R6Class("Beta",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Beta",
    short_name = "Beta",
    description = "Beta Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param shape1 `(numeric(1))`\cr
    #' First shape parameter, `shape1 > 0`.
    #' @param shape2 `(numeric(1))`\cr
    #' Second shape parameter, `shape2 > 0`.
    initialize = function(shape1 = 1, shape2 = 1, decorators = NULL) {

      private$.parameters <- getParameterSet.Beta(self, shape1, shape2)
      self$setParameterValue(shape1 = shape1, shape2 = shape2)

      super$initialize(
        decorators = decorators,
        support = Interval$new(0, 1),
        symmetry = if (shape1 == shape2) "sym" else "asym",
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
      s1 <- unlist(self$getParameterValue("shape1"))
      s2 <- unlist(self$getParameterValue("shape2"))

      return(s1 / (s1 + s2))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      s1 <- unlist(self$getParameterValue("shape1"))
      s2 <- unlist(self$getParameterValue("shape2"))

      if (length(s1) > 1) {
        if (which == "all") {
          stop("`which` cannot be `'all'` when vectorising.")
        } else {
          mode <- rep(NaN, length(s1))
          mode[s1 <= 1 & s2 > 1] <- 0
          mode[s1 > 1 & s2 <= 1] <- 1
          mode[s1 < 1 & s2 < 1] <- c(0, 1)[which]
          mode[s1 > 1 & s2 > 1] <- (s1 - 1) / (s1 + s2 - 2)
          return(mode)
        }
      } else {
        if (s1 <= 1 & s2 > 1) {
          return(0)
        } else if (s1 > 1 & s2 <= 1) {
          return(1)
        } else if (s1 < 1 & s2 < 1) {
          if (which == "all") {
            return(c(0, 1))
          } else {
            return(c(0, 1)[which])
          }
        } else if (s1 > 1 & s2 > 1) {
          return((s1 - 1) / (s1 + s2 - 2))
        } else {
          return(NaN)
        }
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      shape1 <- unlist(self$getParameterValue("shape1"))
      shape2 <- unlist(self$getParameterValue("shape2"))
      return(shape1 * shape2 * ((shape1 + shape2)^-2) * (shape1 + shape2 + 1)^-1)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      shape1 <- unlist(self$getParameterValue("shape1"))
      shape2 <- unlist(self$getParameterValue("shape2"))
      return(2 * (shape2 - shape1) * ((shape1 + shape2 + 1)^0.5) * ((shape1 + shape2 + 2)^-1) *
        ((shape1 * shape2)^-0.5))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      shape1 <- unlist(self$getParameterValue("shape1"))
      shape2 <- unlist(self$getParameterValue("shape2"))

      ex_kurtosis <- 6 *
        {
          ((shape1 - shape2)^2) * (shape1 + shape2 + 1) - (shape1 * shape2 * (shape1 + shape2 + 2))
        } /
        (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))
      if (excess) {
        return(ex_kurtosis)
      } else {
        return(ex_kurtosis + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      shape1 <- unlist(self$getParameterValue("shape1"))
      shape2 <- unlist(self$getParameterValue("shape2"))
      return(log(beta(shape1, shape2), base) - ((shape1 - 1) * digamma(shape1)) -
        ((shape2 - 1) * digamma(shape2)) + ((shape1 + shape2 - 2) * digamma(shape1 + shape2)))
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
      if (self$getParameterValue("shape1") == self$getParameterValue("shape2")) {
        private$.properties$symmetry <- "symmetric"
      } else {
        private$.properties$symmetry <- "asymmetric"
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "dbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        log = log,
        vec = test_list(shape1)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "pbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "qbeta",
        x = p,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .rand = function(n) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "rbeta",
        x = n,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        vec = test_list(shape1)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Beta", ClassName = "Beta",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
