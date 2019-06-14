#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Poisson Distribution Documentation
#-------------------------------------------------------------
#' @title Poisson Distribution
#'
#' @description Mathematical and statistical functions for the Poisson distribution parameterised
#' with arrival rate.
#'
#' @name Poisson
#'
#' @section Constructor: Poisson$new(rate = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{rate} \tab numeric \tab arrival rate. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Poisson distribution is parameterised with (arrival) rate as a
#' positive numeric.
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Binomial Statistical Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#'
#' @export
NULL
#-------------------------------------------------------------
# Poisson Distribution Definition
#-------------------------------------------------------------
Poisson <- R6::R6Class("Poisson", inherit = SDistribution, lock_objects = F)
Poisson$set("public","name","Poisson")
Poisson$set("public","short_name","Pois")
Poisson$set("public","traits",list(type = PosIntegers$new(zero = T),
                                   valueSupport = "discrete",
                                   variateForm = "univariate"))

Poisson$set("public","description","Poisson Probability Distribution.")

Poisson$set("public","mean",function(){
  return(self$getParameterValue("rate"))
})
Poisson$set("public","var",function(){
  return(self$getParameterValue("rate"))
})
Poisson$set("public","skewness",function(){
  return(self$getParameterValue("rate")^(-0.5))
})
Poisson$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(1/self$getParameterValue("rate"))
  else
    return(1/self$getParameterValue("rate") + 3)
})
Poisson$set("public", "mgf", function(t){
  return(exp(self$getParameterValue("rate")*(exp(t)-1)))
})
Poisson$set("public", "cf", function(t){
  return(exp(self$getParameterValue("rate")*(exp(1i*t)-1)))
})
Poisson$set("public","pgf",function(z){
  return(exp(self$getParameterValue("rate")*(z-1)))
})
Poisson$set("public","entropy",function(){
  message("No analytic result for Possion entropy available. Try decorating with CoreStatistics.")
  return(NULL)
})

Poisson$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  return(lst)
})

Poisson$set("public","initialize",function(rate = 1, decorators = NULL, verbose = FALSE, ...){

  private$.parameters <- getParameterSet(self, rate, verbose)
  self$setParameterValue(list(rate = rate))

  if(rate>=30)
    symmetric <- TRUE
  else
    symmetric <- FALSE


  pdf <- function(x1) dpois(x1, self$getParameterValue("rate"))
  cdf <- function(x1) ppois(x1, self$getParameterValue("rate"))
  quantile <- function(p) qpois(p, self$getParameterValue("rate"))
  rand <- function(n) rpois(n, self$getParameterValue("rate"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosIntegers$new(zero = T),
                   distrDomain = PosIntegers$new(zero = T),
                   symmetric = symmetric)


  invisible(self)
})
