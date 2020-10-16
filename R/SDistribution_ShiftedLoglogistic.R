# nolint start
#' @name ShiftedLoglogistic
#' @template SDist
#' @templateVar ClassName ShiftedLoglogistic
#' @templateVar DistName Shifted Log-Logistic
#' @templateVar uses in survival analysis for its non-monotonic hazard as well as in economics, a generalised variant of [Loglogistic]
#' @templateVar params shape, \eqn{\beta}, scale, \eqn{\alpha}, and location, \eqn{\gamma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta/\alpha)((x-\gamma)/\alpha)^{\beta-1}(1 + ((x-\gamma)/\alpha)^\beta)^{-2}}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0} and \eqn{\gamma >= 0}
#' @templateVar distsupport the non-negative Reals
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_ratescale
#' @template param_location
#' @template param_shape
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
ShiftedLoglogistic <- R6Class("ShiftedLoglogistic",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "ShiftedLoglogistic",
    short_name = "ShiftLLogis",
    description = "Shifted Loglogistic Probability Distribution.",
    packages = "pracma",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(scale = 1, shape = 1, location = 0,
                          rate = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, scale, shape, location, rate)
      self$setParameterValue(scale = scale, shape = shape, location = location, rate = rate)

      if (self$getParameterValue("shape") == 0) {
        support <- Reals$new()
      } else if (self$getParameterValue("shape") < 0) {
        support <- Interval$new(-Inf, self$getParameterValue("location") -
          self$getParameterValue("scale") / self$getParameterValue("shape"),
        type = "(]"
        )
      } else {
        support <- Interval$new(self$getParameterValue("location") -
          self$getParameterValue("scale") / self$getParameterValue("shape"),
        Inf,
        type = "[)"
        )
      }

      super$initialize(
        decorators = decorators,
        support = support,
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
      location <- unlist(self$getParameterValue("location"))
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))

      return(location + ((scale / shape) * (((pi * shape) / (sin(pi * shape))) - 1)))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      location <- unlist(self$getParameterValue("location"))
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))

      return(location + ((scale / shape) * ((((1 - shape) / (1 + shape))^shape) - 1)))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      unlist(self$getParameterValue("location"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))
      shapi <- pi * unlist(self$getParameterValue("shape"))

      return((scale^2 / shape^2) * ((2 * shapi / sin(2 * shapi)) - ((shapi / sin(shapi))^2)))
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
      if (!is.null(lst$rate)) lst$scale <- NULL
      super$setParameterValue(lst = lst, error = error)
      if (self$getParameterValue("shape") == 0) {
        private$.properties$support <- Reals$new()
      } else if (self$getParameterValue("shape") < 0) {
        private$.properties$support <- Interval$new(-Inf, self$getParameterValue("location") -
          self$getParameterValue("scale") / self$getParameterValue("shape"),
        type = "(]"
        )
      } else {
        private$.properties$support <- Interval$new(self$getParameterValue("location") -
          self$getParameterValue("scale") / self$getParameterValue("shape"),
        Inf,
        type = "[)"
        )
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      location <- self$getParameterValue("location")
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      if (checkmate::testList(location)) {
        return(C_ShiftedLoglogisticPdf(x, unlist(location), unlist(shape), unlist(scale), log))
      } else {
        return(as.numeric(C_ShiftedLoglogisticPdf(x, location, shape, scale, log)))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("location")
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      if (checkmate::testList(location)) {
        return(C_ShiftedLoglogisticCdf(x, unlist(location), unlist(shape), unlist(scale),
                                       lower.tail, log.p))
      } else {
        return(as.numeric(C_ShiftedLoglogisticCdf(x, location, shape, scale, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("location")
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      if (checkmate::testList(location)) {
        return(C_ShiftedLoglogisticQuantile(p, unlist(location), unlist(shape), unlist(scale),
                                            lower.tail, log.p))
      } else {
        return(as.numeric(C_ShiftedLoglogisticQuantile(p, location, shape, scale, lower.tail,
                                                       log.p)))
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
    ShortName = "ShiftLLogis", ClassName = "ShiftedLoglogistic",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "-", Tags = ""
  )
)
