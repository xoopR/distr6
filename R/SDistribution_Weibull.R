#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Weibull Distribution Documentation
#-------------------------------------------------------------
#' @title Weibull Distribution
#'
#' @description Mathematical and statistical functions for the Weibull distribution parameterised
#' with shape and scale. The Weibull distribution is defined by the pdf,
#' \deqn{f(x) = (\alpha/\beta)*(x/\beta)^(\alpha-1)*exp(-x/\beta)^\alpha, x \ge 0; 0 otherwise}
#' where \eqn{\alpha > 0} is the shape parameter and \eqn{\beta > 0} is the scale parameter.
#'
#' @details \code{mgf} and \code{cf} are omitted as no closed form analytic expressions could be found.
#'
#' @name Weibull
#'
#' @section Constructor: Weibull$new(shape = 1, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Weibull distribution is parameterised with a shape and scale
#' parameter, both as positive numerics.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- Weibull$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 1))
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
# Weibull Distribution Definition
#-------------------------------------------------------------
Weibull <- R6::R6Class("Weibull", inherit = SDistribution, lock_objects = F)
Weibull$set("public","name","Weibull")
Weibull$set("public","short_name","Weibull")
Weibull$set("public","traits",list(type = PosReals$new(zero=T),
                                   valueSupport = "continuous",
                                   variateForm = "univariate"))
Weibull$set("public","description","Weibull Probability Distribution.")
Weibull$set("public","package","stats")

Weibull$set("public","mean",function(){
  return(self$getParameterValue("scale")*gamma(1+1/self$getParameterValue("shape")))
})
Weibull$set("public","var",function(){
  scale<-self$getParameterValue("scale")
  shape<-self$getParameterValue("shape")
  return(scale^2 *(gamma(1+2/shape)-gamma(1+1/shape)^2))
})
Weibull$set("public","skewness",function() {
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  mu <- self$mean()
  sigma <- self$sd()
  return(((gamma(1+3/shape)*(scale^3))  - (3*mu*sigma^2) - (mu^3)) / (sigma^3))
})
Weibull$set("public","kurtosis",function(excess = TRUE){
  skew <- self$skewness()
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  mu <- self$mean()
  sigma <- self$sd()

  kur <- (((scale^4) * gamma(1+4/shape)) - (4*skew*(sigma^3)*mu) - (6*(sigma^2)*(mu^2)) - (mu^4)) / (sigma^4)

  if(excess)
    return(kur - 3)
  else
    return(kur)
})
Weibull$set("public","entropy",function(base = 2){
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  return(-digamma(1)*(1-1/shape)+log(scale/shape, base)+1)
})
Weibull$set("public","mode",function(){
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")

  if(shape > 1)
    return (scale*((shape-1)/shape)^(1/shape))
  else
    return(0)
})


Weibull$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape=paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale=paramlst$scale))
  return(lst)
})

Weibull$set("public","initialize",function(shape = 1, scale= 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, verbose)
  self$setParameterValue(list(shape=shape, scale = scale))

  pdf <- function(x1) dweibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  cdf <- function(x1) pweibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  quantile <- function(p) qweibull(p, self$getParameterValue("shape"), self$getParameterValue("scale"))
  rand <- function(n) rweibull(n, self$getParameterValue("shape"), self$getParameterValue("scale"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric = FALSE)

  invisible(self)
})
