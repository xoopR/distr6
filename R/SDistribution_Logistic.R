
#-------------------------------------------------------------
# Logistic Distribution Documentation
#-------------------------------------------------------------
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
#' @templateVar constructor mean = 0, scale = 1, sd = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{sd} \tab numeric \tab standard deviation, alternate scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric and either \code{scale} or \code{sd} as positive numerics. These are related via, \deqn{sd = scale*\pi/\sqrt(3)} If \code{sd} is given then {scale} is ignored.
#'
#' @examples
#' x <- Logistic$new(mean = 2, scale = 3)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(sd = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
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
# Logistic Distribution Definition
#-------------------------------------------------------------
Logistic <- R6Class("Logistic", inherit = SDistribution, lock_objects = F)
Logistic$set("public", "name", "Logistic")
Logistic$set("public", "short_name", "Logis")
Logistic$set("public", "description", "Logistic Probability Distribution.")
Logistic$set("public", "packages", "stats")

Logistic$set("public", "mean", function() {
  return(self$getParameterValue("mean"))
})
Logistic$set("public", "variance", function() {
  return(self$getParameterValue("sd")^2)
})
Logistic$set("public", "skewness", function() {
  return(0)
})
Logistic$set("public", "kurtosis", function(excess = TRUE) {
  if (excess) {
    return(6 / 5)
  } else {
    return(6 / 5 + 3)
  }
})
Logistic$set("public", "entropy", function(base = 2) {
  return(2 + log(self$getParameterValue("scale"), base))
})
Logistic$set("public", "mgf", function(t) {
  if (-1 / self$getParameterValue("scale") < t & t < 1 / self$getParameterValue("scale")) {
    return(exp(self$getParameterValue("mean") * t) * beta(1 - self$getParameterValue("scale") * t, 1 + self$getParameterValue("scale") * t))
  } else {
    return(NaN)
  }
})
Logistic$set("public", "pgf", function(z) {
  return(NaN)
})
Logistic$set("public", "cf", function(t) {
  return(exp(1i * self$getParameterValue("mean") * t) *
    (self$getParameterValue("scale") * pi * t) / (sinh(pi * self$getParameterValue("scale") * t)))
})
Logistic$set("public", "mode", function(which = NULL) {
  return(self$getParameterValue("mean"))
})

Logistic$set("private", ".getRefParams", function(paramlst) {
  lst <- list()
  if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
  if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
  if (!is.null(paramlst$sd)) lst <- c(lst, list(scale = paramlst$sd * sqrt(3) / pi))
  return(lst)
})
Logistic$set("private", ".pdf", function(x, log = FALSE) {
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
})
Logistic$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE) {
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
})
Logistic$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE) {
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
})
Logistic$set("private", ".rand", function(n) {
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
})
Logistic$set("private", ".log", TRUE)
Logistic$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

Logistic$set("public", "initialize", function(mean = 0, scale = 1, sd = NULL,
                                              decorators = NULL, verbose = FALSE) {

  private$.parameters <- getParameterSet(self, mean, scale, sd, verbose)
  self$setParameterValue(mean = mean, scale = scale, sd = sd)

  super$initialize(
    decorators = decorators,
    support = Reals$new(),
    symmetry = "sym",
    type = Reals$new()
  )
})

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Logis", ClassName = "Logistic",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
