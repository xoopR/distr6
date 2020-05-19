
#-------------------------------------------------------------
# Gumbel Distribution Documentation
#-------------------------------------------------------------
#' @name Gumbel
#' @template SDist
#' @templateVar ClassName Gumbel
#' @templateVar DistName Gumbel
#' @templateVar uses to model the maximum (or minimum) of a number of samples of different distributions, and is a special case of the Generalised Extreme Value distribution
#' @templateVar params location, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(z + exp(-z)))/\beta}
#' @templateVar paramsupport \eqn{z = (x-\mu)/\beta}, \eqn{\mu \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar additionalDetails Apery's Constant to 16 significant figures is used in the skewness calculation. The \code{gammaz} function from the \code{pracma} package is used in the \code{cf} to allow complex inputs.
#' @templateVar constructor location = 0, scale = 1
#' @templateVar arg1 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{location} as a numeric and \code{scale} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Frechet}} and \code{\link{Weibull}} for other special cases of the generalized extreme value distribution. \code{\link[pracma]{gammaz}} for the references for the gamma function with complex inputs.
#'
#' @examples
#' x <- Gumbel$new(location = 2, scale = 5)
#'
#' # Update parameters
#' x$setParameterValue(scale = 3)
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
# Gumbel Distribution Definition
#-------------------------------------------------------------
Gumbel <- R6Class("Gumbel", inherit = SDistribution, lock_objects = F)
Gumbel$set("public", "name", "Gumbel")
Gumbel$set("public", "short_name", "Gumb")
Gumbel$set("public", "description", "Gumbel Probability Distribution.")
Gumbel$set("public", "packages", c("extraDistr", "pracma"))

Gumbel$set("public", "mean", function() {
  return(self$getParameterValue("location") - digamma(1) * self$getParameterValue("scale"))
})
Gumbel$set("public", "variance", function() {
  return((pi * self$getParameterValue("scale"))^2 / 6)
})
Gumbel$set("public", "skewness", function() {
  return((12 * sqrt(6) * 1.202056903159594285399738161511449990764986292) / pi^3)
})
Gumbel$set("public", "kurtosis", function(excess = TRUE) {
  if (excess) {
    return(2.4)
  } else {
    return(5.4)
  }
})
Gumbel$set("public", "entropy", function(base = 2) {
  return(log(self$getParameterValue("scale"), base) - digamma(1) + 1)
})
Gumbel$set("public", "mgf", function(t) {
  return(gamma(1 - self$getParameterValue("scale") * t) * exp(self$getParameterValue("location") * t))
})
Gumbel$set("public", "pgf", function(z) {
  return(NaN)
})
Gumbel$set("public", "cf", function(t) {
  return(pracma::gammaz(1 - self$getParameterValue("scale") * t * 1i) * exp(1i * self$getParameterValue("location") * t))
})
Gumbel$set("public", "mode", function(which = NULL) {
  return(self$getParameterValue("location"))
})
Gumbel$set("private", ".getRefParams", function(paramlst) {
  lst <- list()
  if (!is.null(paramlst$location)) lst <- c(lst, list(location = paramlst$location))
  if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
  return(lst)
})

Gumbel$set("private", ".pdf", function(x, log = FALSE) {
  if (checkmate::testList(self$getParameterValue("location"))) {
    mapply(
      extraDistr::dgumbel,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      MoreArgs = list(x = x, log = log)
    )
  } else {
    extraDistr::dgumbel(
      x,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      log = log
    )
  }
})
Gumbel$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = TRUE) {
  if (checkmate::testList(self$getParameterValue("location"))) {
    mapply(
      extraDistr::pgumbel,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      MoreArgs = list(
        q = x,
        lower.tail = lower.tail,
        log.p = log.p
      )
    )
  } else {
    extraDistr::pgumbel(
      x,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      lower.tail = lower.tail,
      log.p = log.p
    )
  }
})
Gumbel$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = TRUE) {
  if (checkmate::testList(self$getParameterValue("location"))) {
    mapply(
      extraDistr::qgumbel,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      MoreArgs = list(
        p = p,
        lower.tail = lower.tail,
        log.p = log.p
      )
    )
  } else {
    extraDistr::qgumbel(
      p,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      lower.tail = lower.tail,
      log.p = log.p
    )
  }
})
Gumbel$set("private", ".rand", function(n) {
  if (checkmate::testList(self$getParameterValue("location"))) {
    mapply(
      extraDistr::rgumbel,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale"),
      MoreArgs = list(n = n)
    )
  } else {
    extraDistr::rgumbel(
      n,
      mu = self$getParameterValue("location"),
      sigma = self$getParameterValue("scale")
    )
  }
})
Gumbel$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

Gumbel$set("public", "initialize", function(location = 0, scale = 1,
                                            decorators = NULL, verbose = FALSE) {

  private$.parameters <- getParameterSet(self, location, scale, verbose)
  self$setParameterValue(location = location, scale = scale)

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
    ShortName = "Gumb", ClassName = "Gumbel",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
