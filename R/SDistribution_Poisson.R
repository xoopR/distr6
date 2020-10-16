# nolint start
#' @name Poisson
#' @template SDist
#' @templateVar ClassName Poisson
#' @templateVar DistName Poisson
#' @templateVar uses to model the number of events occurring in at a constant, independent rate over an interval of time or space
#' @templateVar params arrival rate, \eqn{\lambda},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (\lambda^x * exp(-\lambda))/x!}
#' @templateVar paramsupport \eqn{\lambda} > 0
#' @templateVar distsupport the Naturals
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_rate
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Poisson <- R6Class("Poisson",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Poisson",
    short_name = "Pois",
    description = "Poisson Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(rate = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, rate)
      self$setParameterValue(rate = rate)

      super$initialize(
        decorators = decorators,
        support = Naturals$new(),
        type = Naturals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("rate"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      sapply(self$getParameterValue("rate"), floor)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("rate"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      unlist(self$getParameterValue("rate"))^(-0.5) # nolint
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
        return(1 / unlist(self$getParameterValue("rate")))
      } else {
        return(1 / unlist(self$getParameterValue("rate")) + 3)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(exp(self$getParameterValue("rate") * (exp(t) - 1)))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp(self$getParameterValue("rate") * (exp(1i * t) - 1)))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(exp(self$getParameterValue("rate") * (z - 1)))
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dpois",
        x = x,
        args = list(lambda = unlist(lambda)),
        log = log,
        vec = test_list(lambda)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "ppois",
        x = x,
        args = list(lambda = unlist(lambda)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(lambda)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qpois",
        x = p,
        args = list(lambda = unlist(lambda)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(lambda)
      )
    },
    .rand = function(n) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rpois",
        x = n,
        args = list(lambda = unlist(lambda)),
        vec = test_list(lambda)
      )
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Pois", ClassName = "Poisson",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
