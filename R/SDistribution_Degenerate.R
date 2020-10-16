# nolint start
#' @name Degenerate
#' @template SDist
#' @templateVar ClassName Degenerate
#' @templateVar DistName Degenerate
#' @templateVar uses to model deterministic events or as a representation of the delta, or Heaviside, function
#' @templateVar params mean, \eqn{\mu}
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1, \ if \ x = \mu}{f(x) = 1, if x = \mu}\deqn{f(x) = 0, \ if \ x \neq \mu}{f(x) = 0, if x != \mu}
#' @templateVar paramsupport \eqn{\mu \epsilon R}
#' @templateVar distsupport \eqn{{\mu}}
#' @templateVar aka Dirac
#' @aliases Dirac Delta
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#'
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
Degenerate <- R6Class("Degenerate",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Degenerate",
    short_name = "Degen",
    description = "Degenerate Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `numeric(1)` \cr
    #' Mean of the distribution, defined on the Reals.
    initialize = function(mean = 0, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean)
      self$setParameterValue(mean = mean)

      super$initialize(
        decorators = decorators,
        support = Set$new(mean, class = "numeric"),
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
      numeric(length(self$getParameterValue("mean")))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("mean")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      rep(NaN, length(self$getParameterValue("mean")))
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      numeric(length(self$getParameterValue("mean")))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(exp(self$getParameterValue("mean") * t))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(exp(self$getParameterValue("mean") * t * 1i))
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      private$.properties$support <- Set$new(self$getParameterValue("mean"), class = "numeric")
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegeneratePdf(x, unlist(mean), log))
      } else {
        return(as.numeric(C_DegeneratePdf(x, mean, log)))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegenerateCdf(x, unlist(mean), lower.tail, log.p))
      } else {
        return(as.numeric(C_DegenerateCdf(x, mean, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegenerateQuantile(p, unlist(mean), lower.tail, log.p))
      } else {
        return(as.numeric(C_DegenerateQuantile(p, mean, lower.tail, log.p)))
      }
    },
    .rand = function(n) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(data.table(matrix(rep(mean, n), nrow = n)))
      } else {
        return(rep(mean, n))
      }
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Degen", ClassName = "Degenerate",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = "limits"
  )
)
