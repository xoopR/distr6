#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gamma Distribution Documentation
#-------------------------------------------------------------
#' @title Gamma Distribution
#'
#' @description Mathematical and statistical functions for the Gamma distribution parameterised
#' with shape and rate, \eqn{scale = 1/rate} or \eqn{mean = shape/rate}. The shape-rate Gamma distribution is defined by the pdf,
#' \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha) * x^(\alpha-1) * exp(-x\beta)}
#' where \eqn{\alpha > 0} is the shape parameter, \eqn{\beta > 0} is the rate parameter and
#' \eqn{\Gamma} is the gamma function.
#'
#' @details The Gamma Distribution is parameterised by default with shape and rate as this is most common
#' in statistics (particularly Bayesian).
#'
#' @name Gamma
#'
#' @section Constructor: Gamma$new(shape = 1,rate = 1, scale = NULL, mean = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{rate} \tab numeric \tab inverse scale parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{mean} \tab numeric \tab alternate scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Gamma distribution can either be parameterised with shape and
#' rate, scale or mean. If none are provided then the default is taken to be rate = 1 and shape = 1.
#' If multiple are provided then parameterisation takes the hierarchy: mean, scale, rate.
#' Scale is defined by
#' \deqn{scale = rate^-1}
#' Mean is defined by
#' \deqn{mean = shape/rate}
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' Gamma$new(shape = 1, rate = 2)
#' Gamma$new(shape = 1, scale = 4)
#' Gamma$new(shape = 1, mean = 0.5)
#'
#' x = Gamma$new(verbose = TRUE) # Default is shape = 1, rate = 1
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
# Gamma Distribution Definition
#-------------------------------------------------------------
Gamma <- R6::R6Class("Gamma", inherit = SDistribution, lock_objects = F)
Gamma$set("public","name","Gamma")
Gamma$set("public","short_name","Gamma")

Gamma$set("public","traits",list(type = PosReals$new(),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))

Gamma$set("public","description","Gamma Probability Distribution.")
Gamma$set("public","package","stats")

Gamma$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Gamma$set("public","var",function(){
  return(self$getParameterValue("mean")*self$getParameterValue("scale"))
})
Gamma$set("public","skewness",function() {
  2/sqrt(self$getParameterValue("shape"))
})
Gamma$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6/self$getParameterValue("shape"))
  else
    return((6/self$getParameterValue("shape"))+3)
})
Gamma$set("public","entropy",function(base = 2){
  self$getParameterValue("shape") - log(self$getParameterValue("rate"), base) +
    log(gamma(self$getParameterValue("shape")), base) + (1-self$getParameterValue("shape"))*digamma(self$getParameterValue("shape"))
})
Gamma$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return((1-self$getParameterValue("scale")*t)^(-self$getParameterValue("shape")))
  else
    return(NaN)
})
Gamma$set("public", "cf", function(t){
  return((1-self$getParameterValue("scale")*1i*t)^(-self$getParameterValue("shape"))   )
})
Gamma$set("public","mode",function(){
  if(self$getParameterValue("shape")>=1)
    return((self$getParameterValue("shape")-1)/self$getParameterValue("rate"))
  else
    return(NaN)
})

Gamma$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))
  if(!is.null(paramlst$mean)){
    if(is.null(paramlst$shape))
      lst = c(lst, list(rate = self$getParameterValue("shape")/paramlst$mean))
    else
      lst = c(lst, list(rate = paramlst$shape/paramlst$mean))
  }

  return(lst)
})

Gamma$set("public","initialize",function(shape = 1,rate = 1, scale = NULL, mean = NULL, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.Gamma(self, shape, rate, scale, mean, verbose)
  self$setParameterValue(list(shape=shape,rate=rate,scale = scale,mean=mean))

  pdf <- function(x1) dgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  cdf <- function(x1) pgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  quantile <- function(p) qgamma(p, self$getParameterValue("shape"),self$getParameterValue('rate'))
  rand <- function(n) rgamma(n, self$getParameterValue("shape"),self$getParameterValue('rate'))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = F), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})










