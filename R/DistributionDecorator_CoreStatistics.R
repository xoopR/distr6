#' @title Core Statistical Methods for Distributions
#'
#' @description Added functionality to distribution objects for numerical statistical
#'   methods. Including a generalised expectation function for more complex numerical calculations.
#'
#' @name CoreStatistics
#'
#' @param distribution distribution object.
#' @param t integer. Input for function evaluation.
#' @param base integer. Logarithmic base for entropy
#' @param excess logical. If TRUE (default) Excess Kurtosis is calculated
#' @param k integer. Moment to calculate.
#' @param type string. One of "central" or "standard".
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution. See the help pages for the individual
#'  CoreStatistics methods to learn more.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#'
#'  Generating functions are evaluated at a particular point \code{t} and do not
#'   give specific analytic generating functions. \code{type} of moment is one of,
#'   "central" for the kth moment about the mean, or "standard" for the central moment
#'   standardised by, kthCentralMoment / standard deviation^k.
#'
#' @seealso \code{\link{decorate}} for the decorate function and \code{\link{ExoticStatistics}} for
#' more available methods.
#'
#' @examples
#' x = Binomial$new()
#' decorate(x, CoreStatistics, R62S3 = FALSE)
#' x$iqr()
#'
#' @examples
#' x = Binomial$new(decorators = CoreStatistics, R62S3 = FALSE)
#' x$kthmoment(4)
NULL


#' @export
CoreStatistics <- R6::R6Class("CoreStatistics", inherit = DistributionDecorator)

#' @title Moment Generating Function
#' @name mgf
#' @section Usage: $mgf(t)
#' @return \code{mgf} gives the moment generating function evaluated at t
CoreStatistics$set("public", "mgf", function(t) {
  return(self$genExp(trafo = function(x) {return(exp(x*t))}))
})

#' @rdname CoreStatistics
#' @name cf
#' @section Usage: $cf(t)
#' @return \code{cf} gives the characteristic function evaluated at t
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self)){
    return(self$genExp(trafo = function(x) {return(exp(x*t*(1+0i)))}))
  }
})

#' @rdname CoreStatistics
#' @name pgf
#' @section Usage: $pgf(t)
#' @return \code{pgf} gives the probability generating function evaluated at t
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$genExp(trafo = function(x) {return(z^x)})
    return(x)
  }
})

#' @rdname CoreStatistics
#' @name iqr
#' @section Usage: $iqr()
#' @return \code{iqr} gives the interquartile range of the distribution
CoreStatistics$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})

#' @rdname CoreStatistics
#' @name entropy
#' @section Usage: entropy(base = 2)
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
#' @section Usage: $skewness()
#' @return \code{skewness} gives the 3rd standardised moment of a distribution.
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})

#' @rdname CoreStatistics
#' @name kurtosis
#' @section Usage: $kurtosis(excess = TRUE)
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
#' @name kthmoment
#' @section Usage: $kthmoment(k, type = "central")
#' @return \code{kthmoment} gives the kth central (default) or standardized moment
CoreStatistics$set("public", "kthmoment", function(k, type = "central"){

  if(testUnivariate(self)){
    if(type == "central"){
      if(k == 0)
        return(1)
      if(k == 1)
        return(0)
    }

    centralMoment = self$genExp(trafo = function(x) return((x - self$genExp())^k))

    if(type == "central")
      return(centralMoment)
    else if(type == "standard")
      return(centralMoment / self$sd()^k)
  }
})

#' @rdname CoreStatistics
#' @name genExp
#' @section Usage: $genExp(trafo)
#' @return \code{genExp} gives the expectation (default)
CoreStatistics$set("public","genExp",function(trafo){
  if(missing(trafo)){
    trafo = function(x) return(x)
  }
  if(testDiscrete(self)){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = private$.getWorkingSupportRange()
    pdfs = self$pdf(rng)
    xs = trafo(rng)
    xs[pdfs==0] = 0
    return(sum(pdfs * xs))
  } else if(testContinuous(self)){
    message("Results from numerical integration are approximate only, better results may be available.")
    return(suppressMessages(integrate(function(x) {
      pdfs = self$pdf(x)
      xs = trafo(x)
      xs[pdfs==0] = 0
      return(xs * pdfs)
    }, lower = self$inf(), upper = self$sup())$value))
  }
}) # IN PROGRESS


#' @rdname CoreStatistics
#' @name var
#' @section Usage: $var()
#' @return \code{var} gives the variance
CoreStatistics$set("public","var",function(){
  return(self$genExp(trafo = function(x) x^2) - self$genExp()^2)
}) # IN PROGRESS

#' @rdname CoreStatistics
#' @name cov
#' @section Usage: $cov()
#' @return \code{cov} gives the covariance
CoreStatistics$set("public","cov",function(){
  if(testUnivariate(self))
    return(self$var())
}) # TO DO

#' @rdname CoreStatistics
#' @name cor
#' @section Usage: $cor()
#' @return \code{cor} gives the correlation
CoreStatistics$set("public","cor",function(){}) # TO DO

#' @rdname CoreStatistics
#' @name mode
#' @section Usage: $mode()
#' @return \code{mode} gives the mode
CoreStatistics$set("public","mode",function(which = 1){
  if(which==1){
    if(testDiscrete(self)){
      rng = try(self$inf():self$sup(),silent = T)
      if(inherits(rng,"try-error"))
        rng = self$getWorkingSupport()
      return(rng[which.max(self$pdf(rng))])
    } else if(testContinuous(self))
      return(optimize(self$pdf,c(self$inf(),1e08), maximum = TRUE))
  }
}) # IN PROGRESS
