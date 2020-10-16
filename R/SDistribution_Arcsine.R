#' @name Arcsine
#' @template SDist
#' @templateVar ClassName Arcsine
#' @templateVar DistName Arcsine
#' @templateVar uses in the study of random walks and as a special case of the Beta distribution
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(\pi\sqrt{(x-a)(b-x))}}
#' @templateVar paramsupport \eqn{-\infty < a \le b < \infty}
#' @templateVar distsupport \eqn{[a, b]}
#'
#' @template param_lower
#' @template param_upper
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Arcsine <- R6Class("Arcsine",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Arcsine",
    short_name = "Arc",
    description = "Arcsine Probability Distribution.",

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
      return((unlist(self$getParameterValue("upper")) + unlist(self$getParameterValue("lower"))) /
        2)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      if (checkmate::testList(lower)) {
        modes <- data.table(lower, upper)
        if (which == "all") {
          return(modes)
        } else {
          return(unlist(modes[, ..which]))
        }
      } else {
        if (which == "all") {
          return(c(lower, upper))
        } else {
          return(c(lower, upper)[which])
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
      ((unlist(self$getParameterValue("upper")) - unlist(self$getParameterValue("lower")))^2) / 8
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      rep(0, length(self$getParameterValue("lower")))
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
        return(rep(-1.5, length(self$getParameterValue("lower"))))
      } else {
        return(rep(1.5, length(self$getParameterValue("lower"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      rep(log(pi / 4, base), length(self$getParameterValue("lower")))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      NaN
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
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsinePdf(x, unlist(lower), unlist(upper), log))
      } else {
        return(as.numeric(C_ArcsinePdf(x, lower, upper, log)))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsineCdf(x, unlist(lower), unlist(upper), lower.tail, log.p))
      } else {
        return(as.numeric(C_ArcsineCdf(x, lower, upper, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsineQuantile(p, unlist(lower), unlist(upper), lower.tail, log.p))
      } else {
        return(as.numeric(C_ArcsineQuantile(p, lower, upper, lower.tail, log.p)))
      }
    },
    .rand = function(n) {
      self$quantile(runif(n))
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Arc", ClassName = "Arcsine",
    Type = "\u211D", ValueSupport = "continuous", VariateForm = "univariate",
    Package = "-", Tags = "limits"
  )
)
