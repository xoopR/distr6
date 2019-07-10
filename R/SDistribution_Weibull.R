#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Weibull Distribution Documentation
#-------------------------------------------------------------
#' @name Weibull
#' @template SDist
#' @templateVar ClassName Weibull
#' @templateVar DistName Weibull
#' @templateVar uses in survival analysis and is a special case of the Generalized Extreme Value distribution
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)(x/\beta)^{\alpha-1}exp(-x/\beta)^\alpha, x \ge 0; 0 otherwise}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link{Frechet}} and \code{\link{Gumbel}} for other special cases of the generalized extreme value distribution.
#'
#' @examples
#' x <- Weibull$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 1))
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
# Weibull Distribution Definition
#-------------------------------------------------------------
Weibull <- R6::R6Class("Weibull", inherit = SDistribution, lock_objects = F)
Weibull$set("public","name","Weibull")
Weibull$set("public","short_name","Weibull")
Weibull$set("public","description","Weibull Probability Distribution.")
Weibull$set("public","package","stats")

Weibull$set("public","mean",function(){
  return(self$getParameterValue("scale")*gamma(1+1/self$getParameterValue("shape")))
})
Weibull$set("public","variance",function(){
  scale<-self$getParameterValue("scale")
  shape<-self$getParameterValue("shape")
  return(scale^2 *(gamma(1+2/shape)-gamma(1+1/shape)^2))
})
Weibull$set("public","skewness",function() {
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  mu <- self$mean()
  sigma <- self$stdev()
  return(((gamma(1+3/shape)*(scale^3))  - (3*mu*sigma^2) - (mu^3)) / (sigma^3))
})
Weibull$set("public","kurtosis",function(excess = TRUE){
  skew <- self$skewness()
  scale <- self$getParameterValue("scale")
  shape <- self$getParameterValue("shape")
  mu <- self$mean()
  sigma <- self$stdev()

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
                   rand = rand, support = PosReals$new(zero = T),
                   symmetric = FALSE, type = PosReals$new(zero=T), valueSupport = "continuous",
                   variateForm = "univariate")

  invisible(self)
})
