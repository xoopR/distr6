#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gamma Distribution Documentation
#-------------------------------------------------------------
#' @title Gamma Distribution
#'
#' @description Mathematical and statistical functions for the Gamma distribution parameterised
#' with mean and shape, rate or scale. By default we use the shape/rate combination, which is most common
#' in statistics (particularly Bayesian). The shape/rate Gamma distribution is defined by the pdf,
#' \deqn{f(x) = (b^a)/Gamma(a) * x^(a-1) * exp(-bx)}
#' where a is the shape parameter and b is the rate parameter.
#'
#' @details All results given are analytical but we do make use of the base R gamma function, which is
#' found in common usage R and we therefore trust the precision of the results.
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
#' @section Constructor Details: The Gamma distribution can either be parameterised with rate,
#' scale or mean. If none are provided then the default is taken to be rate = 1 and shape = 1.
#' If multiple are provided then parameterisation takes the hierarchy: mean, scale, rate.
#' Scale is defined by
#' \deqn{scale = rate^-1}
#' Mean is defined by
#' \deqn{mean = shape/rate}
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
Gamma <- R6::R6Class("Gamma", inherit = SDistribution, lock_objects = F)
Gamma$set("public","name","Gamma")
Gamma$set("public","short_name","Gam")

Gamma$set("public","traits",list(type = PosReals$new(zero = T),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))

Gamma$set("public","description","Gamma Probability Distribution.")

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
  if(!is.null(paramlst$mean)) lst = c(lst, list(rate = paramlst$shape/paramlst$mean))

  return(lst)
})

Gamma$set("public","initialize",function(shape = 1,rate = 1, scale = NULL, mean = NULL, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.Gamma(self, shape, scale, mean, rate, verbose)
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










