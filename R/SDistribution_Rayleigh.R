
#' @name Rayleigh
#' @template SDist
#' @templateVar ClassName Rayleigh
#' @templateVar DistName Rayleigh
#' @templateVar uses to model random complex numbers.
#' @templateVar params mode (or scale), \eqn{\alpha},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = x/\alpha^2 exp(-x^2/(2\alpha^2))}
#' @templateVar paramsupport \eqn{\alpha > 0}
#' @templateVar distsupport \eqn{[0, \infty)}
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
Rayleigh <- R6Class("Rayleigh",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Rayleigh",
    short_name = "Rayl",
    description = "Rayleigh Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mode `(numeric(1))`\cr
    #' Mode of the distribution, defined on the positive Reals. Scale parameter.
    initialize = function(mode = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mode)
      self$setParameterValue(mode = mode)

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
      unlist(self$getParameterValue("mode")) * sqrt(pi / 2)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("mode"))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      unlist(self$getParameterValue("mode")) * sqrt(2 * log(2))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      (4 - pi) / 2 * unlist(self$getParameterValue("mode"))^2
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      rep((2 * sqrt(pi) * (pi - 3)) / ((4 - pi)^(3 / 2)), length(self$getParameterValue("mode"))) # nolint
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
        return(rep(-(6 * pi^2 - 24 * pi + 16) / (4 - pi)^2, length(self$getParameterValue("mode")))) # nolint
      } else {
        return(rep(-(6 * pi^2 - 24 * pi + 16) / (4 - pi)^2 + 3, # nolint
                   length(self$getParameterValue("mode"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      1 + log(unlist(self$getParameterValue("mode")) / sqrt(2), base) - digamma(1) / 2
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
      if (checkmate::testList(self$getParameterValue("mode"))) {
        mapply(
          extraDistr::drayleigh,
          sigma = self$getParameterValue("mode"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::drayleigh(
          x,
          sigma = self$getParameterValue("mode"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mode"))) {
        mapply(
          extraDistr::prayleigh,
          sigma = self$getParameterValue("mode"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::prayleigh(
          x,
          sigma = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mode"))) {
        mapply(
          extraDistr::qrayleigh,
          sigma = self$getParameterValue("mode"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qrayleigh(
          p,
          sigma = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("mode"))) {
        mapply(
          extraDistr::rrayleigh,
          sigma = self$getParameterValue("mode"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rrayleigh(
          n,
          sigma = self$getParameterValue("mode")
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
    ShortName = "Rayl", ClassName = "Rayleigh",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "scale"
  )
)
