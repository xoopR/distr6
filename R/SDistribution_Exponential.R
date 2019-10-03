#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Exponential Distribution Documentation
#-------------------------------------------------------------
#' @name Exponential
#' @template SDist
#' @templateVar ClassName Exponential
#' @templateVar DistName Exponential
#' @templateVar uses to model inter-arrival times in a Poisson process and has the memoryless property
#' @templateVar params rate, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \lambda exp(-x\lambda)}
#' @templateVar paramsupport \eqn{\lambda > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor rate = NULL, scale = NULL
#' @templateVar arg1 \code{rate} \tab numeric \tab arrival rate. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets  \code{rate} or \code{scale} as positive numerics. These are related via, \deqn{scale = 1/rate} If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Exponential$new(rate = 4)
#' Exponential$new(scale = 3)
#'
#' x = Exponential$new(verbose = TRUE) # Default is rate = 1
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(scale = 2)
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
#'
#' @export
NULL
#-------------------------------------------------------------
# Exponential Distribution Definition
#-------------------------------------------------------------
Exponential <- R6::R6Class("Exponential", inherit = SDistribution, lock_objects = F)
Exponential$set("public","name","Exponential")
Exponential$set("public","short_name","Exp")
Exponential$set("public","description","Exponential Probability Distribution.")
Exponential$set("public","package","stats")

Exponential$set("public","mean",function(){
  self$getParameterValue("scale")
})
Exponential$set("public","variance",function(){
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
    return(NaN)
})
Exponential$set("public", "cf", function(t){
  return(self$getParameterValue("rate") / (self$getParameterValue("rate") -  ((0+1i) * t)))
})
Exponential$set("public","mode",function(){
  return(0)
})
Exponential$set("public", "pgf", function(z){
  return(NaN)
})

Exponential$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))
  return(lst)
})

Exponential$set("public","initialize",function(rate = 1, scale = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, rate, scale, verbose)
  self$setParameterValue(rate = rate, scale = scale)

  pdf <- function(x1) dexp(x1, self$getParameterValue("rate"))
  cdf <- function(x1) pexp(x1, self$getParameterValue("rate"))
  quantile <- function(p) qexp(p, self$getParameterValue("rate"))
  rand <- function(n) rexp(n, self$getParameterValue("rate"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T),
                   symmetric  = FALSE, type = PosReals$new(zero = T),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Exp", ClassName = "Exponential",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
