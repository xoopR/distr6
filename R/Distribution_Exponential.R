#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Exponential Distribution Documentation
#-------------------------------------------------------------
#' @title Exponential Distribution
#' @description Mathematical and statistical functions for the Exponential distribution parameterised
#' with rate or scale.
#' @name Exponential
#'
#' @section Constructor: Exponential$new(rate = NULL, scale = NULL, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{rate} \tab numeric \tab arrival rate. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' }
#'
#' @section Constructor Details: The exponential distribution can either be parameterised with a rate or
#' scale parameter. If neither are provided then rate parameterisation is used with rate = 1. If both are
#' provided then rate parameterisation is used with given rate. Scale is defined by
#' \deqn{scale = 1/rate}
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Normal Statistical Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @export
NULL
#-------------------------------------------------------------
# Exponential Distribution Definition
#-------------------------------------------------------------
Exponential <- R6::R6Class("Exponential", inherit = Distribution, lock_objects = F)
Exponential$set("public","name","Exponential")
Exponential$set("public","short_name","Exp")
Exponential$set("public","traits",list(type = PosReals$new(zero = T),
                                    valueSupport = "continuous",
                                    variateForm = "univariate"))
Exponential$set("public","description","Exponential Probability Distribution.")

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
Exponential$set("public","initialize",function(rate = NULL, scale = NULL, decorators = NULL){

  rate.bool = FALSE
  scale.bool = FALSE

  if(is.null(rate) & is.null(scale)){
    message("rate and scale missing. rate = 1 parameterisation used.")
    rate = 1
  } else if(!is.null(rate) & !is.null(scale)){
    message("Both rate and scale provided. rate parameterisation used.")
    rate = rate
    scale = NULL
  }

  if(!is.null(rate)){
    rate.bool = TRUE
    rate.update = NA
    scale.update = "self$getParameterValue('rate')^-1"
  } else{
    scale.bool = TRUE
    scale.update = NA
    rate.update = "self$getParameterValue('scale')^-1"
  }

  private$.parameters <- ParameterSet$new(id = list("rate","scale"), value = list(1, 1),
                   lower = list(0, 0), upper = list(Inf, Inf),
                   class = list("numeric","numeric"),
                   settable = list(rate.bool, scale.bool),
                   updateFunc = list(rate.update, scale.update),
                   description = list("Arrival Rate", "Scale"))

  if(!is.null(rate)) self$setParameterValue(list(rate = rate))
  if(!is.null(scale)) self$setParameterValue(list(scale = scale))

  pdf <- function(x1) dexp(x1, self$getParameterValue("rate"))
  cdf <- function(x1) pexp(x1, self$getParameterValue("rate"))
  quantile <- function(p) qexp(p, self$getParameterValue("rate"))
  rand <- function(n) rexp(n, self$getParameterValue("rate"))

  private$.properties
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})
