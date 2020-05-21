
#-------------------------------------------------------------
# Log-Logistic Distribution Documentation
#-------------------------------------------------------------
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
#-------------------------------------------------------------
# ShiftedLoglogistic Distribution Definition
#-------------------------------------------------------------
ShiftedLoglogistic <- R6Class("ShiftedLoglogistic", inherit = SDistribution, lock_objects = F)
ShiftedLoglogistic$set("public", "name", "ShiftedLoglogistic")
ShiftedLoglogistic$set("public", "short_name", "ShiftLLogis")
ShiftedLoglogistic$set("public", "description", "Shifted Log-Logistic Probability Distribution.")
ShiftedLoglogistic$set("public", "packages", "pracma")

ShiftedLoglogistic$set("public", "mean", function() {
  location <- self$getParameterValue("location")
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")

  return(location + ((scale/shape) * (((pi*shape)/(sin(pi*shape))) - 1)))
})
ShiftedLoglogistic$set("public", "median", function() {
  return(self$getParameterValue("location"))
})
ShiftedLoglogistic$set("public", "variance", function() {
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  shapi <- pi * self$getParameterValue("shape")

  return((scale^2/shape^2) * ((2 * shapi / sin(2 * shapi)) - ((shapi/sin(shapi))^2)))
})
ShiftedLoglogistic$set("public", "mode", function(which = NULL) {
  location <- self$getParameterValue("location")
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")

  return(location + ((scale/shape) * ((((1 - shape)/(1 + shape))^shape) - 1)))
})
ShiftedLoglogistic$set("public", "pgf", function(z) {
  return(NaN)
})

ShiftedLoglogistic$set("public", "setParameterValue", function(..., lst = NULL, error = "warn") {
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Interval$new(self$getParameterValue("location"), Inf, type = "()")
  invisible(self)
})

ShiftedLoglogistic$set("private", ".getRefParams", function(paramlst) {
  lst <- list()
  if (!is.null(paramlst$location)) lst <- c(lst, list(location = paramlst$location))
  if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
  if (!is.null(paramlst$rate)) lst <- c(lst, list(scale = paramlst$rate^-1))
  if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
  return(lst)
})
ShiftedLoglogistic$set("private", ".pdf", function(x, log = FALSE) {
  location <- self$getParameterValue("location")
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")

  if (checkmate::testList(location)) {
    return(C_ShiftedLoglogisticPdf(x, unlist(location), unlist(shape), unlist(scale), log))
  } else {
    return(as.numeric(C_ShiftedLoglogisticPdf(x, location, shape, scale, log)))
  }
})
ShiftedLoglogistic$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE) {
  location <- self$getParameterValue("location")
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")

  if (checkmate::testList(location)) {
    return(C_ShiftedLoglogisticCdf(x, unlist(location), unlist(shape), unlist(scale), lower.tail, log.p))
  } else {
    return(as.numeric(C_ShiftedLoglogisticCdf(x, location, shape, scale, lower.tail, log.p)))
  }
})
ShiftedLoglogistic$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE) {
  location <- self$getParameterValue("location")
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")

  if (checkmate::testList(location)) {
    return(C_ShiftedLoglogisticQuantile(p, unlist(location), unlist(shape), unlist(scale), lower.tail, log.p))
  } else {
    return(as.numeric(C_ShiftedLoglogisticQuantile(p, location, shape, scale, lower.tail, log.p)))
  }
})
ShiftedLoglogistic$set("private", ".rand", function(n) {
  self$quantile(runif(n))
})
ShiftedLoglogistic$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

ShiftedLoglogistic$set("public", "initialize", function(scale = 1, shape = 1, location = 0,
                                                        rate = NULL, decorators = NULL, verbose = FALSE) {

  private$.parameters <- getParameterSet(self, scale, shape, location, rate, verbose)
  self$setParameterValue(scale = scale, shape = shape, location = location, rate = rate)

  super$initialize(
    decorators = decorators,
    support = Interval$new(location, Inf, type = "()"),
    type = PosReals$new(zero = T)
  )
})

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "ShiftLLogis", ClassName = "ShiftedLoglogistic",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "-"
  )
)
