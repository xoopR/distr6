#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Exponential Distribution Documentation
#-------------------------------------------------------------
#' @title Exponential Distribution
#'
#' @description Mathematical and statistical functions for the Exponential distribution parameterised
#' with rate or \eqn{scale = 1/rate}. The rate parameterisation is defined by the pdf,
#' \deqn{f(x) = \lambda exp(-x\lambda)}
#' where \eqn{\lambda > 0} is the rate parameter.
#'
#' @details By default we use the rate parameterisation for the Exponential distribution as arrival rate
#' provides the most interpretable representation of the distribution and is most popular for say point
#' processes.
#'
#' @name Exponential
#'
#' @section Constructor: Exponential$new(rate = NULL, scale = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{rate} \tab numeric \tab arrival rate. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The exponential distribution can either be parameterised with a rate or
#' scale parameter. Default parameterisation is with rate = 1. If scale is given then rate is ignored.
#' Scale is defined by
#' \deqn{scale = 1/rate}
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' Exponential$new(rate = 4)
#' Exponential$new(scale = 3)
#'
#' x = Exponential$new(verbose = TRUE) # Default is rate = 1
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 2)) # When any parameter is updated, all others are too!
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$var()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Exponential Distribution Definition
#-------------------------------------------------------------
Exponential <- R6::R6Class("Exponential", inherit = SDistribution, lock_objects = F)
Exponential$set("public","name","Exponential")
Exponential$set("public","short_name","Exp")
Exponential$set("public","traits",list(type = PosReals$new(zero = T),
                                    valueSupport = "continuous",
                                    variateForm = "univariate"))
Exponential$set("public","description","Exponential Probability Distribution.")
Exponential$set("public","package","stats")

Exponential$set("public","mean",function(){
  self$getParameterValue("scale")
})
Exponential$set("public","var",function(){
  self$getParameterValue("scale")^2
})
Exponential$set("public","skewness",function() return(2))
Exponential$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6)
  else
    return(9)
})
Exponential$set("public","entropy",function(base = 2){
  1 - log(self$getParameterValue("rate"), base)
})
Exponential$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return(self$getParameterValue("rate") / (self$getParameterValue("rate") - t))
  else
    return(0)
})
Exponential$set("public", "cf", function(t){
  return(self$getParameterValue("rate") / (self$getParameterValue("rate") -  ((0+1i) * t)))
})
Exponential$set("public","mode",function(){
  return(0)
})

Exponential$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))
  return(lst)
})

Exponential$set("public","initialize",function(rate = 1, scale = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, rate, scale, verbose)
  self$setParameterValue(list(rate = rate, scale = scale))

  pdf <- function(x1) dexp(x1, self$getParameterValue("rate"))
  cdf <- function(x1) pexp(x1, self$getParameterValue("rate"))
  quantile <- function(p) qexp(p, self$getParameterValue("rate"))
  rand <- function(n) rexp(n, self$getParameterValue("rate"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})
