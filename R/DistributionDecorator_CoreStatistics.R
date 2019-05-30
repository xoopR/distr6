#' @title Core Statistical Methods for Distributions
#'
#' @description Added functionality to distribution objects for numerical statistical
#'   methods. Including a generalised expectation function for more complex numerical calculations.
#'
#' @name CoreStatistics
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' \code{R62S3} \tab logical \tab If TRUE (default), S3 methods are added for decorators in construction.
#' }
#'
#' @section Public Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input -> Output} \tab \strong{Details} \cr
#' \code{mgf(t)} \tab numeric -> numeric \tab Moment generating function evaluated at t. \cr
#' \code{pgf(t)} \tab numeric -> numeric \tab Probability generating function evaluated at t. \cr
#' \code{cf(t)} \tab numeric -> numeric \tab Characteristic function evaluated at t. \cr
#' \code{iqr()} \tab -> numeric \tab Interquartile range of the distribution. \cr
#' \code{entropy(base = 2)} \tab integer -> numeric \tab Distribution entropy, Shannon by default. \cr
#' \code{skewness()} \tab -> numeric \tab Third stndardised moment of the distribution. \cr
#' \code{kurtosis(excess = TRUE)} \tab logical -> numeric \tab Fourth standardised moment of the distribution. \cr
#' By default excess kurtosis (kurtosis - 3). \cr
#' \code{kthmoment(type = "central")} \tab character -> numeric \tab Central (default) or standardised kth moment (type = "standard"). \cr
#' \code{genExp(trafo)} \tab function -> numeric \tab Generalised expectation formula. If trafo = NULL, returns arithmetic mean. \cr
#' \code{var()} \tab -> numeric \tab Numeric variance of distribution. \cr
#' \code{cov()} \tab -> numeric \tab Covariance of distribution. \cr
#' \code{cor()} \tab -> numeric \tab Correlation of distribution. \cr
#' \code{mode(which = 1)} \tab integer -> numeric \tab Mode of distribution, if which = 1, returns first, otherwise all.
#' }
#'
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @seealso \code{\link{DistributionDecorator}}
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
CoreStatistics$set("public", "mgf", function(t) {
  return(self$genExp(trafo = function(x) {return(exp(x*t))}))
})
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self)){
    return(self$genExp(trafo = function(x) {return(exp(x*t*(1+0i)))}))
  }
})
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$genExp(trafo = function(x) {return(z^x)})
    return(x)
  }
})
CoreStatistics$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})
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
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis = self$kthmoment(k = 4, type = "standard")
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})
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
CoreStatistics$set("public","genExp",function(trafo = NULL){
  if(is.null(trafo)){
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
})
CoreStatistics$set("public","var",function(){
  return(self$genExp(trafo = function(x) x^2) - self$genExp()^2)
})
CoreStatistics$set("public","cov",function(){
  if(testUnivariate(self))
    return(self$var())
}) # TO DO
CoreStatistics$set("public","cor",function(){}) # TO DO
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
