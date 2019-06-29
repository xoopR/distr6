#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Rayleigh Distribution Documentation
#-------------------------------------------------------------
#' @title Rayleigh Distribution
#'
#' @description Mathematical and statistical functions for the Rayleigh distribution parameterised
#' with mode and defined by the pdf,
#' \deqn{f(x) = x/\sigma^2 exp(-x^2/(2\sigma^2))}
#' where \eqn{\sigma > 0} is the mode parameter.
#'
#' @details The cf and mgf are omitted as no closed form analytic expression could be found.
#'
#' @name Rayleigh
#'
#' @section Constructor: Rayleigh$new(mode = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mode} \tab numeric \tab mode, scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Rayleigh distribution is parameterised by default with mode = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- Rayleigh$new(mode = 2)
#'
#' # Update parameters
#' x$setParameterValue(list(mode = 4))
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(1:4)
#' x$cdf(2)
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
# Rayleigh Distribution Definition
#-------------------------------------------------------------
Rayleigh <- R6::R6Class("Rayleigh", inherit = SDistribution, lock_objects = F)
Rayleigh$set("public","name","Rayleigh")
Rayleigh$set("public","short_name","Rayl")
Rayleigh$set("public","traits",list(type = PosReals$new(zero = T),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Rayleigh$set("public","description","Rayleigh Probability Distribution.")
Rayleigh$set("public","package","distr6")

Rayleigh$set("public","mean",function(){
  return(self$getParameterValue("mode")*sqrt(pi/2))
})
Rayleigh$set("public","mode",function(){
  return(self$getParameterValue("mode"))
})
Rayleigh$set("public","var",function(){
  return((4-pi)/2 * self$getParameterValue("mode")^2)
})
Rayleigh$set("public","skewness",function(){
  return((2*sqrt(pi)*(pi-3))/((4-pi)^(3/2)))
})
Rayleigh$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-(6*pi^2 - 24*pi + 16)/(4-pi)^2)
  else
    return(-(6*pi^2 - 24*pi + 16)/(4-pi)^2 + 3)
})
Rayleigh$set("public","entropy",function(base = 2){
  return(1 + log(self$getParameterValue("mode")/sqrt(2), base) - digamma(1)/2)
})

Rayleigh$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mode)) lst = c(lst, list(mode = paramlst$mode))
  return(lst)
})

Rayleigh$set("public","initialize",function(mode = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mode, verbose)
  self$setParameterValue(list(mode = mode))

  pdf <- function(x1){
    return(x1/self$getParameterValue("mode")^2 * exp((-x1^2)/(2*self$getParameterValue("mode")^2)))
  }
  cdf <- function(x1){
    return(1 - exp((-x1^2)/(2*self$getParameterValue("mode")^2)))
  }
  quantile <- function(p){
    return(self$getParameterValue("mode")*sqrt(-2*log(1-p)))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric = FALSE)
  invisible(self)
})
