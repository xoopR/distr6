#' @title Core Statistics Methods for Distributions
#'
#' @description Added functionality to distribution objects for statistical
#'   methods that can be considered core but lie outside of the p/d/q/r generation
#'   functions.
#' @name CoreStatistics
#'
#' @usage $new(distribution)
#' @return \code{new} constructs an R6 object of class Distribution.
#'
#' @param distribution distribution object.
#' @param t integer. Input for function evaluation.
#' @param base integer. Logarithmic base for entropy
#' @param excess logical. If TRUE (default) Excess Kurtosis is calculated
#' @param k integer. Moment to calculate.
#' @param type string. One of "central" or "standard".
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by overwriting the object in the Global Environment. They can be specified
#'  in construction of the Distribution or by constructing the given Decorator.
#'
#'  Methods act on the distribution and not the constructor therefore method chaining of the form
#'  \code{CoreStatistics$new(distribution)$iqr()} is not supported but \code{distribution$new(decorator=CoreStatistics)$iqr()} is.
#'
#'  Generating functions are evaluated at a particular point \code{t} and do not
#'   give specific analytic generating functions. \code{type} of moment is one of,
#'   "central" for the kth moment about the mean, or "standard" for the central moment
#'   standardised by, kthCentralMoment / standard deviation^k.
#'
#'   Many functions rely on the expectation of a distribution and therefore results may be
#'   approximate when numerical integration is used.
#'
#' @seealso \code{\link{ExoticStatistics}} for more available methods.
#'
#' @examples
#' X = Binomial$new(decorator = "CoreStatistics")
#' X$iqr()
#' X$kurtosis()
#'
#' @examples
#' X = Binomial$new()
#' CoreStatistics$new(X)
#' X$kthmoment(4)
NULL


#' @export
CoreStatistics <- R6::R6Class("CoreStatistics", inherit = DistributionDecorator)

#' @rdname CoreStatistics
#' @name mgf
#' @usage $mgf(t)
#' @return \code{mgf} gives the moment generating function evaluated at t
CoreStatistics$set("public", "mgf", function(t) {
  return(self$expectation(trafo = function(x) {return(exp(x*t))}))
})

#' @rdname CoreStatistics
#' @name cf
#' @usage $cf(t)
#' @return \code{cf} gives the characteristic function evaluated at t
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self)){
    return(self$expectation(trafo = function(x) {return(exp(x*t*(1+0i)))}))
  }
})

#' @rdname CoreStatistics
#' @name pgf
#' @usage $pgf(t)
#' @return \code{pgf} gives the probability generating function evaluated at t
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$expectation(trafo = function(x) {return(z^x)})
    return(x)
  }
})

#' @rdname CoreStatistics
#' @name iqr
#' @usage $iqr()
#' @return \code{iqr} gives the interquartile range of the distribution
CoreStatistics$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})

#' @rdname CoreStatistics
#' @name entropy
#' @usage $entropy(base = 2)
#' @return \code{entropy} gives the (information) entropy of a distribution,
#'   default is Shannon entropy (base = 2)
CoreStatistics$set("public", "entropy", function(base = 2) {
  if(testDiscrete(self)){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = getWorkingSupport(self)
    probs = self$pdf(rng)
    logs = log(self$pdf(rng), base)
    return(-sum(probs * logs))
  } else if(testContinuous(self)){
    warning("Results from numerical integration are approximate only, better results may be available.")
    return(-integrate(function(x) {
      probs = self$pdf(x)
      logs = log(self$pdf(x), base)
      logs[probs==0] = 0
      return(probs * logs)
    }, lower = self$inf(), upper = self$sup())$value)
  }
})

#' @rdname CoreStatistics
#' @name skewness
#' @usage $skewness()
#' @return \code{skewness} gives the 3rd standardised moment of a distribution.
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})

#' @rdname CoreStatistics
#' @name skewnessType
#' @usage $skewnessType()
#' @return \code{skewnessType} is an accessor for the type of Skewness
CoreStatistics$set("public", "skewnessType", function() {
  return(self$.__enclos_env__$private$.properties$skewness)
})

#' @rdname CoreStatistics
#' @name kurtosis
#' @usage $kurtosis(excess = TRUE)
#' @return \code{kurtosis} gives the 4th standardised moment of a distribution.
#'   Excess (kurtosis - 3) is default.
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis = self$kthmoment(k = 4, type = "standard")
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})

#' @rdname CoreStatistics
#' @name kurtosisType
#' @usage $kurtosisType()
#' @return \code{kurtosisType} is an accessor for the type of Kurtosis
CoreStatistics$set("public", "kurtosisType", function() {
  return(self$.__enclos_env__$private$.properties$kurtosis)
})

#' @rdname CoreStatistics
#' @name kthmoment
#' @usage $kthmoment(k, type = "central")
#' @return \code{kthmoment} gives the kth central (default) or standardized moment
CoreStatistics$set("public", "kthmoment", function(k, type = "central"){

  if(testUnivariate(self)){
    if(type == "central"){
      if(k == 0)
        return(1)
      if(k == 1)
        return(0)
    }

    centralMoment = self$expectation(trafo = function(x) return((x - self$expectation())^k))

    if(type == "central")
      return(centralMoment)
    else if(type == "standard")
      return(centralMoment / self$sd()^k)
  }
})