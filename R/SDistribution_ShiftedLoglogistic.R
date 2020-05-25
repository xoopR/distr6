
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
#' @templateVar omittedVars \code{entropy}, \code{mgf}, \code{skewness}, `kurtosis`, and \code{cf}
#' @templateVar aka Fisk
#' @aliases Fisk
#' @templateVar constructor scale = 1, shape = 1, location = 0
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics and \code{location} as a numeric.
#' @templateVar additionalSeeAlso [Logistic], [Loglogistic].
#'
#' @examples
#' x <- ShiftedLoglogistic$new(shape = 2, scale = 3, location = 2)
#'
#' # Update parameters
#' x$setParameterValue(scale = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5:6)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
NULL

ShiftedLoglogistic <- R6Class("ShiftedLoglogistic", inherit = SDistribution, lock_objects = F,
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

      super$initialize(
        decorators = decorators,
        support = Interval$new(location, Inf, type = "()"),
        type = PosReals$new(zero = T)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")

      return(location + ((scale/shape) * (((pi*shape)/(sin(pi*shape))) - 1)))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = NULL) {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")

      return(location + ((scale/shape) * ((((1 - shape)/(1 + shape))^shape) - 1)))
    },
    median = function() {
      return(self$getParameterValue("location"))
    },
    variance = function() {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")
      shapi <- pi * self$getParameterValue("shape")

      return((scale^2/shape^2) * ((2 * shapi / sin(2 * shapi)) - ((shapi/sin(shapi))^2)))
    },
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      private$.properties$support <- Interval$new(self$getParameterValue("location"), Inf, type = "()")
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
        return(C_ShiftedLoglogisticCdf(x, unlist(location), unlist(shape), unlist(scale), lower.tail, log.p))
      } else {
        return(as.numeric(C_ShiftedLoglogisticCdf(x, location, shape, scale, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("location")
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      if (checkmate::testList(location)) {
        return(C_ShiftedLoglogisticQuantile(p, unlist(location), unlist(shape), unlist(scale), lower.tail, log.p))
      } else {
        return(as.numeric(C_ShiftedLoglogisticQuantile(p, location, shape, scale, lower.tail, log.p)))
      }
    },
    .rand = function(n) {
      self$quantile(runif(n))
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$location)) lst <- c(lst, list(location = paramlst$location))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      if (!is.null(paramlst$rate)) lst <- c(lst, list(scale = paramlst$rate^-1))
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
      return(lst)
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
    Package = "-"
  )
)
