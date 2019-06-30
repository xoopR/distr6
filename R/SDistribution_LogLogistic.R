#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Log-Logistic Distribution Documentation
#-------------------------------------------------------------
#' @title Log-Logistic Distribution
#'
#' @description Mathematical and statistical functions for the Log-Logistic (aka Fisk) distribution parameterised
#' with shape, scale and location, and defined by the pdf
#' \deqn{f(x) = (\beta/\alpha)((x-\gamma)/\alpha)^{\beta-1}(1 + ((x-\gamma)/\alpha)^\beta)^{-2}}
#' where \eqn{\beta > 0} is the shape parameter, \eqn{\alpha > 0} is the scale parameter and
#' \eqn{\gamma >= 0} is the location parameter.
#'
#' @details One of the difficulties in the log-logistic distribution is that there is little concensus
#' online about the most common parameterisation and indeed often these are incorrectly conflated. We
#' have therefore only included one parameterisation, which is in-line with McLaughlin's Compendium (2016).
#'
#' Entropy, mgf and cf are omitted as no closed form analytic expression could be found.
#'
#' @references
#'
#' McLaughlin, M. P. (2016). Compendium of Common Probability Distributions.
#'
#' @name LogLogistic
#'
#' @section Constructor: LogLogistic$new(scale = 1, shape = 1, location = 0, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{location} \tab numeric \tab location parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The LogLogistic distribution is parameterised by default with
#' shape = 1, scale = 1 and location = 0.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- LogLogistic$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 2))
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5:6)
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
# LogLogistic Distribution Definition
#-------------------------------------------------------------
LogLogistic <- R6::R6Class("LogLogistic", inherit = SDistribution, lock_objects = F)
LogLogistic$set("public","name","LogLogistic")
LogLogistic$set("public","short_name","LLogis")
LogLogistic$set("public","traits",list(type = PosReals$new(zero = T),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
LogLogistic$set("public","description","LogLogistic Probability Distribution.")
LogLogistic$set("public","package","distr6")

LogLogistic$set("public","mean",function(){
  return(self$getParameterValue("location") +
           (self$getParameterValue("scale")*pi/self$getParameterValue("shape"))/
           sin(pi/self$getParameterValue("shape")))
})
LogLogistic$set("public","var",function(){
  if(self$getParameterValue("shape") > 2){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    return(scale^2 * ((2*shapi)/sin(2*shapi) - (shapi^2)/sin(shapi)^2))
  } else
    return(NaN)
})
LogLogistic$set("public","skewness",function(){
  if(self$getParameterValue("shape") > 3){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    s1 <- (2*shapi^3*scale^3)/sin(shapi)^3
    s2 <- (6*shapi^2*scale^3)*(1/sin(shapi))*(1/sin(2*shapi))
    s3 <- (3*shapi*scale^3)/sin(3*shapi)
    return(s1-s2+s3)
  } else
    return(NaN)
})
LogLogistic$set("public","kurtosis",function(excess = TRUE){
  if(self$getParameterValue("shape") > 4){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    s1 <- (3*shapi^4*scale^4)/sin(shapi)^4
    s2 <- (12*shapi^3*scale^4)*(1/sin(shapi)^2)*(1/sin(2*shapi))
    s3 <- (12*shapi^2*scale^4)*(1/sin(shapi))*(1/sin(3*shapi))
    s4 <- (4*shapi*scale^4)*(1/sin(4*shapi))
    kurtosis = -s1 + s2 - s3 + s4
    if(excess)
      return(kurtosis - 3)
    else
      return(kurtosis)
  } else
    return(NaN)
})
LogLogistic$set("public","mode",function(){
  shape <- self$getParameterValue("shape")
  return(self$getParameterValue("location") +
           self$getParameterValue("scale")*((shape-1)/(shape+1))^(1/shape))
})

LogLogistic$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  private$.properties$support <- Interval$new(self$getParameterValue("location"),Inf,type="()")
  invisible(self)
})

LogLogistic$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  return(lst)
})

LogLogistic$set("public","initialize",function(scale = 1, shape = 1, location = 0,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, scale, shape, location, verbose)
  self$setParameterValue(list(scale = scale, shape = shape, location = location))

  pdf <- function(x1){
    location <- self$getParameterValue("location")
    shape <- self$getParameterValue("shape")
    scale <- self$getParameterValue("scale")

    return((shape/scale) * (((x1 - location)/scale)^(shape-1)) * (1 + ((x1 - location)/scale)^shape)^-2)
  }
  cdf <- function(x1){
    return((1 + ((x1 -self$getParameterValue("location"))/self$getParameterValue("scale"))^-self$getParameterValue("shape"))^-1)
  }
  quantile <- function(p){
    return(self$getParameterValue("scale")*(p/(1-p))^(1/self$getParameterValue("shape")) + self$getParameterValue("location"))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(location,Inf,type="()"), distrDomain = PosReals$new(zero=T),
                   symmetric = FALSE)
  invisible(self)
})
