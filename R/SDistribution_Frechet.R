#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Frechet Distribution Documentation
#-------------------------------------------------------------
#' @title Frechet Distribution Class
#'
#' @description Mathematical and statistical functions for the Frechet (Inverse Weibull) distribution,
#' which is used as a special case of the Generalised Extreme Value distribution.
#'
#' @details The Frechet distribution parameterised with shape, \eqn{\alpha}, scale, \eqn{\beta},
#' and minimum, \eqn{\gamma}, is defined by the pdf,
#' \deqn{f(x) = (\alpha/\beta)((x-\gamma)/\beta)^{-1-\alpha}exp(-(x-\gamma)/\beta)^{-\alpha}}
#' for \eqn{\alpha, \beta \epsilon R^+} and \eqn{\gamma \epsilon R}.
#'
#' The distribution is supported on \eqn{x > \gamma}.
#'
#' \code{mgf} and \code{cf} are omitted as no closed form analytical expression could be found. Decorate with
#' \code{CoreStatistics} for numerical results.
#'
#' @name Frechet
#'
#' @section Constructor: Frechet$new(shape = 1, scale = 1, minimum = 0,
#' decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{minimum} \tab numeric \tab location parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Frechet distribution is parameterised with
#' \code{shape}, \code{scale} as positive numerics and \code{minimum} as a numeric.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Frechet$new(shape = 2, scale = 3, minimum = 6)
#'
#' # Update parameters
#' x$setParameterValue(list(shape = 3))
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
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Frechet Distribution Definition
#-------------------------------------------------------------
Frechet <- R6::R6Class("Frechet", inherit = SDistribution, lock_objects = F)
Frechet$set("public","name","Frechet")
Frechet$set("public","short_name","Frec")
Frechet$set("public","description","Frechet Probability Distribution.")
Frechet$set("public","package","distr6")

Frechet$set("public","mean",function(){
  if(self$getParameterValue("shape") <= 1)
    return(Inf)
  else
    return(self$getParameterValue("minimum") + self$getParameterValue("scale")*gamma(1 - 1/self$getParameterValue("shape")))
})
Frechet$set("public","variance",function(){
  if(self$getParameterValue("shape") <= 2)
    return(Inf)
  else
    return(self$getParameterValue("scale")^2 * (gamma(1 - 2/self$getParameterValue("shape")) -
                                                  gamma(1 - 1/self$getParameterValue("shape"))^2))
})
Frechet$set("public","skewness",function(){
  if(self$getParameterValue("shape") <= 3)
    return(Inf)
  else{
    shape <- self$getParameterValue("shape")
    num <- gamma(1-3/shape) - 3*gamma(1 - 2/shape) * gamma(1 - 1/shape) + 2*gamma(1-1/shape)^3
    den <- (gamma(1-2/shape) - gamma(1-1/shape)^2)^(3/2)
    return(num/den)
  }
})
Frechet$set("public","kurtosis",function(excess = TRUE){
  if(self$getParameterValue("shape") <= 4)
    return(Inf)
  else{
    shape <- self$getParameterValue("shape")
    num <- gamma(1-4/shape) - 4*gamma(1 - 3/shape) * gamma(1 - 1/shape) + 3*gamma(1-2/shape)^2
    den <- (gamma(1-2/shape) - gamma(1-1/shape)^2)^2
    if(excess)
      return(-6 + num/den)
    else
      return(-3 + num/den)
  }
})
Frechet$set("public","mode",function(){
  return(self$getParameterValue("minimum") +
    self$getParameterValue("scale")*
      (self$getParameterValue("shape")/
         (1+self$getParameterValue("shape")))^(1/self$getParameterValue("shape")))
})
Frechet$set("public","entropy",function(base = 2){
  return(1 - digamma(1)/self$getParameterValue("shape") - digamma(1) +
           log(self$getParameterValue("scale")/self$getParameterValue("shape"), base))
})

Frechet$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$minimum)) lst = c(lst, list(minimum = paramlst$minimum))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Frechet$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  private$.properties$support <- Interval$new(self$getParameterValue("minimum"),Inf,type="()")
  invisible(self)
})

Frechet$set("public","initialize",function(shape = 1, scale = 1, minimum = 0,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, minimum, verbose)
  self$setParameterValue(list(shape = shape, scale = scale, minimum = minimum))

  pdf <- function(x1){
    scale <- self$getParameterValue("scale")
    shape <- self$getParameterValue("shape")
    minimum <- self$getParameterValue("minimum")
    return(shape/scale * (((x1 - minimum)/scale)^(-1-shape)) * exp(-((x1-minimum)/scale)^-shape))
  }
  cdf <- function(x1){
    return(exp((-(x1-self$getParameterValue("minimum"))/self$getParameterValue("scale"))^-self$getParameterValue("shape")))
  }
  quantile <- function(p){
    return(self$getParameterValue("scale") * (-1/log(p))^(1/self$getParameterValue("shape")) +
             self$getParameterValue("minimum"))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(minimum, Inf, type = "()"), distrDomain = Reals$new(),
                   symmetric = FALSE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
