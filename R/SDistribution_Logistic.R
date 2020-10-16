
#' @name Logistic
#' @template SDist
#' @templateVar ClassName Logistic
#' @templateVar DistName Logistic
#' @templateVar uses in logistic regression and feedforward neural networks
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{s},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(x-\mu)/s) / (s(1+exp(-(x-\mu)/s))^2)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{s > 0}
#' @templateVar distsupport the Reals
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
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Logistic <- R6Class("Logistic",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Logistic",
    short_name = "Logis",
    description = "Logistic Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `(numeric(1))`\cr
    #' Mean of the distribution, defined on the Reals.
    #' @param sd `(numeric(1))`\cr
    #' Standard deviation of the distribution as an alternate scale parameter,
    #'  `sd = scale*pi/sqrt(3)`. If given then `scale` is ignored.
    initialize = function(mean = 0, scale = 1, sd = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, scale, sd)
      self$setParameterValue(mean = mean, scale = scale, sd = sd)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
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
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("sd"))^2
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("sd")))
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
        return(rep(1.2, length(self$getParameterValue("sd"))))
      } else {
        return(rep(4.2, length(self$getParameterValue("sd"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      2 + log(unlist(self$getParameterValue("scale")), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (-1 / self$getParameterValue("scale") < t & t < 1 / self$getParameterValue("scale")) {
        return(exp(self$getParameterValue("mean") * t) *
                 beta(1 - self$getParameterValue("scale") * t,
                      1 + self$getParameterValue("scale") * t))
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp(1i * self$getParameterValue("mean") * t) *
        (self$getParameterValue("scale") * pi * t) /
          (sinh(pi * self$getParameterValue("scale") * t)))
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
      if (!is.null(lst$sd)) lst$scale <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "dlogis",
        x = x,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        log = log,
        vec = test_list(location)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "plogis",
        x = x,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(location)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "qlogis",
        x = p,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(location)
      )
    },
    .rand = function(n) {
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "rlogis",
        x = n,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        vec = test_list(location)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Logis", ClassName = "Logistic",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = "locscale"
  )
)
