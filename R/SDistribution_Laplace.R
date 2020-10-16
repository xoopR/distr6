
#' @name Laplace
#' @template SDist
#' @templateVar ClassName Laplace
#' @templateVar DistName Laplace
#' @templateVar uses in signal processing and finance
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-|x-\mu|/\beta)/(2\beta)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\beta > 0}
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
Laplace <- R6Class("Laplace",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Laplace",
    short_name = "Lap",
    description = "Laplace Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `(numeric(1))`\cr
    #' Mean of the distribution, defined on the Reals.
    #' @param var `(numeric(1))`\cr
    #' Variance of the distribution, defined on the positive Reals. `var = 2*scale^2`.
    #' If `var` is provided then `scale` is ignored.
    initialize = function(mean = 0, scale = 1, var = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, scale, var)
      self$setParameterValue(mean = mean, scale = scale, var = var)

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
      unlist(self$getParameterValue("var"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("var")))
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
        rep(3, length(self$getParameterValue("var")))
      } else {
        rep(6, length(self$getParameterValue("var")))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      log(2 * exp(1) * unlist(self$getParameterValue("scale")), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (abs(t) < 1 / self$getParameterValue("scale")) {
        return(exp(self$getParameterValue("mean") * t) /
                 (1 - self$getParameterValue("scale")^2 * t^2))
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp(self$getParameterValue("mean") * t * 1i) /
               (1 + self$getParameterValue("scale")^2 * t^2))
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
      if (!is.null(lst$var)) lst$scale <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::dlaplace,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dlaplace(x,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::plaplace,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::plaplace(x,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::qlaplace,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::qlaplace(p,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::rlaplace,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rlaplace(n,
          mu = self$getParameterValue("mean"),
          sigma = self$getParameterValue("scale")
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
    ShortName = "Lap", ClassName = "Laplace",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "locscale"
  )
)
