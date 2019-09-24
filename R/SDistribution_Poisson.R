#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Poisson Distribution Documentation
#-------------------------------------------------------------
#' @name Poisson
#' @template SDist
#' @templateVar ClassName Poisson
#' @templateVar DistName Poisson
#' @templateVar uses to model the number of events occurring in at a constant, independent rate over an interval of time or space
#' @templateVar params arrival rate, \eqn{\lambda},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (\lambda^x * exp(-\lambda))/x!}
#' @templateVar paramsupport \eqn{\lambda} > 0
#' @templateVar distsupport the Naturals
#' @templateVar omittedVars \code{entropy}
#' @templateVar constructor rate = 1
#' @templateVar arg1 \code{rate} \tab numeric \tab arrival rate. \cr
#' @templateVar constructorDets \code{rate} as a positive numeric.
#'
#'
#' @examples
#' x = Poisson$new(rate = 2)
#'
#' # Update parameters
#' x$setParameterValue(rate = 3)
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
# Poisson Distribution Definition
#-------------------------------------------------------------
Poisson <- R6::R6Class("Poisson", inherit = SDistribution, lock_objects = F)
Poisson$set("public","name","Poisson")
Poisson$set("public","short_name","Pois")
Poisson$set("public","description","Poisson Probability Distribution.")
Poisson$set("public","package","stats")

Poisson$set("public","mean",function(){
  return(self$getParameterValue("rate"))
})
Poisson$set("public","mode",function(){
  return(floor(self$getParameterValue("rate")))
})
Poisson$set("public","variance",function(){
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

Poisson$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  return(lst)
})

Poisson$set("public","initialize",function(rate = 1, decorators = NULL, verbose = FALSE, ...){

  private$.parameters <- getParameterSet(self, rate, verbose)
  self$setParameterValue(rate = rate)

  pdf <- function(x1) dpois(x1, self$getParameterValue("rate"))
  cdf <- function(x1) ppois(x1, self$getParameterValue("rate"))
  quantile <- function(p) qpois(p, self$getParameterValue("rate"))
  rand <- function(n) rpois(n, self$getParameterValue("rate"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Naturals$new(),
                   symmetric = FALSE,type = Naturals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")


  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Pois", ClassName = "Poisson",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
