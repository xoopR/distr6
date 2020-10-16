# nolint start
#' @name Uniform
#' @author Yumi Zhou
#' @template SDist
#' @templateVar ClassName Uniform
#' @templateVar DistName Uniform
#' @templateVar uses to model continuous events occurring with equal probability, as an uninformed prior in Bayesian modelling, and for inverse transform sampling
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(b-a)}
#' @templateVar paramsupport \eqn{-\infty < a < b < \infty}
#' @templateVar distsupport \eqn{[a, b]}
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_lower
#' @template param_upper
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Uniform <- R6Class("Uniform",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Uniform",
    short_name = "Unif",
    description = "Uniform Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(lower = 0, upper = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, lower, upper)
      self$setParameterValue(lower = lower, upper = upper)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper),
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
      (unlist(self$getParameterValue("lower")) + unlist(self$getParameterValue("upper"))) / 2
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      rep(NaN, length(self$getParameterValue("lower")))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      ((unlist(self$getParameterValue("upper")) - unlist(self$getParameterValue("lower")))^2) / 12
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("lower")))
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
        return(rep(-1.2, length(self$getParameterValue("lower"))))
      } else {
        return(rep(1.8, length(self$getParameterValue("lower"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      log(unlist(self$getParameterValue("upper")) - unlist(self$getParameterValue("lower")), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (t == 0) {
        return(1)
      } else {
        return((exp(self$getParameterValue("upper") * t) -
                  exp(self$getParameterValue("lower") * t)) /
          (t * (self$getParameterValue("upper") - self$getParameterValue("lower"))))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      if (t == 0) {
        return(1)
      } else {
        return((exp(self$getParameterValue("upper") * t * 1i) -
                  exp(self$getParameterValue("lower") * t * 1i)) /
          (t * 1i * (self$getParameterValue("upper") - self$getParameterValue("lower"))))
      }
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
      private$.properties$support <- Interval$new(
        self$getParameterValue("lower"),
        self$getParameterValue("upper")
      )
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "dunif",
        x = x,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        log = log,
        vec = test_list(min)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "punif",
        x = x,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(min)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "qunif",
        x = p,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(min)
      )
    },
    .rand = function(n) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "runif",
        x = n,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        vec = test_list(min)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Unif", ClassName = "Uniform",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = "limits"
  )
)
