#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Laplace Distribution Documentation
#-------------------------------------------------------------
#' @title Laplace Distribution
#'
#' @description Mathematical and statistical functions for the Laplace distribution parameterised
#' with mean and scale or \eqn{variance = 2(scale)^2}. The mean/scale parameterisation is defined by
#' the pdf,
#' \deqn{f(x) = 1/2\beta * exp(-|x-\mu|/\beta)}
#' where \eqn{\mu \epsilon R} is the mean parameter and \eqn{\beta > 0} is the scale parameter.
#'
#' @details The Laplace distribution is parameterised with mean and scale by default as this appears to
#' be slightly more common in popular usage.
#'
#' @name Laplace
#'
#' @section Constructor: Laplace$new(mean = 0, scale = 1, var = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab mean, location parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{var} \tab numeric \tab alternate scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Laplace distribution can either be parameterised with mean and
#' scale or variance. The default parameterisation is with mean 0 and scale 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' Laplace$new(scale = 2)
#' Laplace$new(var = 4)
#'
#' x = Laplace$new(verbose = TRUE) # Default is mean = 0, scale = 1
#'
#' # Update parameters
#' x$setParameterValue(list(var = 2)) # When any parameter is updated, all others are too!
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
# Laplace Distribution Definition
#-------------------------------------------------------------
Laplace <- R6::R6Class("Laplace", inherit = SDistribution, lock_objects = F)
Laplace$set("public","name","Laplace")
Laplace$set("public","short_name","Lap")
Laplace$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Laplace$set("public","description","Laplace Probability Distribution.")
Laplace$set("public","package","distr6")

Laplace$set("public","mean",function(){
  self$getParameterValue("mean")
})
Laplace$set("public","var",function(){
  self$getParameterValue("var")
})
Laplace$set("public","skewness",function(){
  return(0)
})
Laplace$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(3)
  else
    return(6)
})
Laplace$set("public","entropy",function(base = 2){
  return(log(2 * exp(1) * self$getParameterValue("scale"), base))
})
Laplace$set("public", "mgf", function(t){
  if(abs(t) < 1/self$getParameterValue("scale"))
    return(exp(self$getParameterValue("mean") * t) / (1 - self$getParameterValue("scale")^2 * t^2))
  else
    return(NaN)
})
Laplace$set("public", "cf", function(t){
  return(exp(self$getParameterValue("mean") * t * 1i) / (1 + self$getParameterValue("scale")^2 * t^2))
})
Laplace$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Laplace$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  if(!is.null(paramlst$var)) lst = c(lst, list(scale = sqrt(paramlst$var/2)))
  return(lst)
})

Laplace$set("public","initialize",function(mean = 0, scale = 1, var = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, scale, var, verbose)
  self$setParameterValue(list(mean = mean, scale = scale, var = var))

  pdf <- function(x1){
    return((2*self$getParameterValue("scale"))^-1 * exp(-abs(x1-self$getParameterValue("mean"))))
  }
  cdf <- function(x1){
    cdf = x1
    cdf[x1 <= self$getParameterValue("mean")] = 0.5 * exp((cdf[x1 <= self$getParameterValue("mean")]-
                                                             self$getParameterValue("mean"))/
                                                            self$getParameterValue("scale"))
    cdf[x1 > self$getParameterValue("mean")] = 1 - (0.5 * exp(-(cdf[x1 > self$getParameterValue("mean")]-
                                                                  self$getParameterValue("mean"))/
                                                                self$getParameterValue("scale")))
    return(cdf)
  }
  quantile <- function(p){
    quantile = p
    quantile[p <= 0.5] = self$getParameterValue("mean") + (self$getParameterValue("scale")*
                                                             log(2*quantile[p <= 0.5]))
    quantile[p > 0.5] = self$getParameterValue("mean") - (self$getParameterValue("scale")*
                                                            log(2 - 2*quantile[p > 0.5]))
    return(quantile)
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Reals$new(), distrDomain = Reals$new(),
                   symmetric = TRUE)
  invisible(self)
})
