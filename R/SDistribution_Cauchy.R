
#' @name Cauchy
#' @author Chijing Zeng
#' @template SDist
#' @templateVar ClassName Cauchy
#' @templateVar DistName Cauchy
#' @templateVar uses in physics and finance
#' @templateVar params location, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1 / (\pi\beta(1 + ((x - \alpha) / \beta)^2))}
#' @templateVar paramsupport \eqn{\alpha \epsilon R} and \eqn{\beta > 0}
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
#' @template param_locationscale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Cauchy <- R6Class("Cauchy",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Cauchy",
    short_name = "Cauchy",
    description = "Cauchy Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(location = 0, scale = 1,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, location, scale)
      self$setParameterValue(location = location, scale = scale)

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
      rep(NaN, length(self$getParameterValue("location")))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("location"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      rep(NaN, length(self$getParameterValue("location")))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      rep(NaN, length(self$getParameterValue("location")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      rep(NaN, length(self$getParameterValue("location")))
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      return(log(4 * pi * unlist(self$getParameterValue("scale")), base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(NaN)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp((self$getParameterValue("location") * 1i * t) -
        (self$getParameterValue("scale") * abs(t))))
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
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "dcauchy",
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
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "pcauchy",
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
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "qcauchy",
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
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "rcauchy",
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
    ShortName = "Cauchy", ClassName = "Cauchy",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = "locscale"
  )
)
