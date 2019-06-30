#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Logarithmic Distribution Documentation
#-------------------------------------------------------------
#' @title Logarithmic Distribution
#'
#' @description Mathematical and statistical functions for the Logarithmic (series) distribution parameterised
#' by a parameter, theta, and defined by the pmf,
#' \deqn{f(x) = -\theta^x/xlog(1-\theta)}
#' where \eqn{0 < theta < 1} is the theta parameter.
#'
#' @details The distribution is implemented by interfacing the extraDistr package, the documentation for
#' the extraDistr distribution can be found here, \code{\link[extraDistr]{LogSeries}}. Entropy
#' is omitted as no closed-form expression could be found, decorate with CoreStatistics for a
#' numeric results.
#'
#' @name Logarithmic
#'
#' @section Constructor: Logarithmic$new(theta = 0.5, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{theta} \tab numeric \tab theta parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Logarithmic distribution is parameterised by default with
#' theta = 0.5.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Logarithmic$new(theta = 0.2)
#'
#' # Update parameters
#' x$setParameterValue(list(theta = 0.3))
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
# Logarithmic Distribution Definition
#-------------------------------------------------------------
Logarithmic <- R6::R6Class("Logarithmic", inherit = SDistribution, lock_objects = F)
Logarithmic$set("public","name","Logarithmic")
Logarithmic$set("public","short_name","Log")
Logarithmic$set("public", "traits",list(type = PosIntegers$new(zero = T),
                                 valueSupport ="discrete",
                                 variateForm = "univariate"))
Logarithmic$set("public","description","Logarithmic Probability Distribution.")
Logarithmic$set("public","package","extraDistr")

Logarithmic$set("public","mean",function(){
  theta = self$getParameterValue("theta")
  return(-theta/(log(1-theta)*(1-theta)))
})
Logarithmic$set("public","var",function(){
  theta = self$getParameterValue("theta")
  return((-theta^2 - theta*log(1-theta)) / ((1-theta)^2 * (log(1-theta))^2))
})
Logarithmic$set("public","mode",function(which = "all"){
  return(1)
})
Logarithmic$set("public","skewness",function(){
  theta = self$getParameterValue("theta")

  s1 = (theta*(3*theta + theta*log(1-theta) + log(1-theta))) / ((theta-1)^3 * log(1-theta)^2)
  s2 = 2 * (-theta/(log(1-theta)*(1-theta)))^3

  return((s1+s2)/(self$sd()^3))
})
Logarithmic$set("public","kurtosis",function(excess = TRUE){
  theta = self$getParameterValue("theta")

  s1 = (3*theta^4)/((1-theta)^4*log(1-theta)^4)
  s2 = (6*theta^3)/((theta-1)^4*log(1-theta)^3)
  s3 = (4*theta^3)/((theta-1)^4*log(1-theta)^2)
  s4 = (theta^3)/((theta-1)^4*log(1-theta))
  s5 = (4*theta^2)/((theta-1)^4*log(1-theta)^2)
  s6 = (4*theta^2)/((theta-1)^4*log(1-theta))
  s7 = (theta)/((theta-1)^4*log(1-theta))

  sum = - s1 - s2 - s3 - s4 - s5 - s6 - s7

  kurtosis = sum/(self$sd()^4)

  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})
Logarithmic$set("public", "mgf", function(t){
  if(t < -log(self$getParameterValue("theta")))
    return(log(1-self$getParameterValue("theta")*exp(t))/log(1-self$getParameterValue("theta")))
  else
    return(NaN)
})
Logarithmic$set("public", "cf", function(t){
  return(log(1-self$getParameterValue("theta")*exp(t*1i))/log(1-self$getParameterValue("theta")))
})
Logarithmic$set("public", "pgf", function(z){
  if(abs(z) < 1/self$getParameterValue("theta"))
    return(log(1-self$getParameterValue("theta")*z)/log(1-self$getParameterValue("theta")))
  else
    return(NaN)
})

Logarithmic$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$theta)) lst = c(lst,list(theta = paramlst$theta))
  return(lst)
})

Logarithmic$set("public", "initialize", function(theta = 0.5, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet.Logarithmic(self, theta, verbose)
  self$setParameterValue(list(theta=theta))

  pdf <- function(x1) extraDistr::dlgser(x1, self$getParameterValue("theta"))
  cdf <- function(x1) extraDistr::plgser(x1, self$getParameterValue("theta"))
  quantile <- function(p) extraDistr::qlgser(p, self$getParameterValue("theta"))
  rand <- function(n) extraDistr::rlgser(n, self$getParameterValue("theta"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosIntegers$new(zero=F), distrDomain = PosReals$new(zero = TRUE),
                   symmetric = FALSE)

  invisible(self)
})
