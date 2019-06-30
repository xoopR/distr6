#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Normal Distribution Documentation
#-------------------------------------------------------------
#' @title Normal Distribution
#'
#' @description Mathematical and statistical functions for the Normal distribution parameterised
#' with mean and variance or \eqn{standard deviation = \sqrt(variance)} or \eqn{precision = 1/variance}.
#' The mean/variance parameterisation is defined by the pdf,
#' \deqn{f(x) = exp(-(x-\mu)^2/(2\sigma^2)) / \sqrt(2\pi\sigma^2)}
#' where \eqn{\mu \epsilon R} is the mean parameter and \eqn{\sigma^2} is the variance parameter.
#'
#' @details Whilst we recognise that the standard deviation parameterisation is used by R stats,
#' we opt for the variance parameterisation as default as this appears slightly more common in popular
#' online resources and textbooks.
#'
#' @name Normal
#'
#' @section Constructor: Normal$new(mean = 0, var = 1, sd = NULL, prec = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab mean, location parameter. \cr
#' \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' \code{precision} \tab numeric \tab precision, squared scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Normal distribution can either be parameterised with variance,
#' standard deviation or precision. If none are provided then var parameterisation is used with var = 1.
#' If multiple are provided then parameterisation takes the hierarchy: var, sd, prec.
#' sd is defined by
#' \deqn{sd = var^2}
#' prec is defined by
#' \deqn{prec = var^-1}
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Different parameterisations
#' Normal$new(var = 1, mean = 1)
#' Normal$new(prec = 2, mean = 1)
#' Normal$new(mean = 1, sd = 2)
#' x <- Normal$new(verbose = TRUE) # Standard normal default
#'
#' # Update parameters
#' x$setParameterValue(list(var = 2))
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
# Normal Distribution Definition
#-------------------------------------------------------------
Normal <- R6::R6Class("Normal", inherit = SDistribution, lock_objects = F)
Normal$set("public","name","Normal")
Normal$set("public","short_name","Norm")
Normal$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Normal$set("public","description","Normal Probability Distribution.")
Normal$set("public","package","stats")

Normal$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Normal$set("public","var",function(){
  return(self$getParameterValue("var"))
})
Normal$set("public","skewness",function(){
  return(0)
})
Normal$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(0)
  else
    return(3)
})
Normal$set("public","entropy",function(base = 2){
  return(0.5 * log(2 * pi * exp(1) * self$getParameterValue("var"), base))
})
Normal$set("public", "mgf", function(t){
  return(exp((self$getParameterValue("mean") * t) + (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public", "cf", function(t){
  return(exp((1i * self$getParameterValue("mean") * t) - (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Normal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$var)) lst = c(lst, list(var = paramlst$var))
  if(!is.null(paramlst$sd)) lst = c(lst, list(var = paramlst$sd^2))
  if(!is.null(paramlst$prec)) lst = c(lst, list(var = paramlst$prec^-1))
  return(lst)
})

Normal$set("public","initialize",function(mean = 0, var = 1, sd = NULL, prec = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, var, sd, prec, verbose)
  self$setParameterValue(list(mean = mean, var = var, sd = sd, prec = prec))

  pdf <- function(x1) dnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  cdf <- function(x1) pnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  quantile <- function(p) qnorm(p, self$getParameterValue("mean"), self$getParameterValue("sd"))
  rand <- function(n) rnorm(n, self$getParameterValue("mean"), self$getParameterValue("sd"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})
