#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gumbel Distribution Documentation
#-------------------------------------------------------------
#' @title Gumbel Distribution
#'
#' @description Mathematical and statistical functions for the Gumbel distribution parameterised
#' with location and scale and defined by the pdf,
#' \deqn{f(x) = 1 / \beta * exp(-(z + exp(-z)))}
#' where \eqn{z = (x-\mu)/\beta}, \eqn{\mu \epsilon R} is the location parameter and \eqn{\beta > 0} is the scale parameter.
#'
#' @name Gumbel
#'
#' @section Constructor: Gumbel$new(location = 0, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{location} \tab numeric \tab location parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Gumbel distribution is parameterised with
#' location and scale. Default parameterisation is with location = 0 and scale = 1.
#'
#' @details Apery's Constant to 16 significant figures is used in the skewness calculation. The gammaz
#' function from the pracma package is used in the cf to allow complex inputs.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Gumbel$new(location = 2, scale = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 3))
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
# Gumbel Distribution Definition
#-------------------------------------------------------------
Gumbel <- R6::R6Class("Gumbel", inherit = SDistribution, lock_objects = F)
Gumbel$set("public","name","Gumbel")
Gumbel$set("public","short_name","Gumb")
Gumbel$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Gumbel$set("public","description","Gumbel Probability Distribution.")
Gumbel$set("public","package","distr6")

Gumbel$set("public","mean",function(){
  return(self$getParameterValue("location") - digamma(1)*self$getParameterValue("scale"))
})
Gumbel$set("public","var",function(){
  return((pi * self$getParameterValue("scale"))^2/6)
})
Gumbel$set("public","skewness",function(){
  return((12*sqrt(6)*1.202056903159594285399738161511449990764986292)/pi^3)
})
Gumbel$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(2.4)
  else
    return(5.4)
})
Gumbel$set("public","entropy",function(base = 2){
  return(log(self$getParameterValue("scale"), base) - digamma(1) + 1)
})
Gumbel$set("public", "mgf", function(t){
  return(gamma(1 - self$getParameterValue("scale")*t) * exp(self$getParameterValue("location")*t))
})
Gumbel$set("public", "cf", function(t){
  return(pracma::gammaz(1 - self$getParameterValue("scale")*t*1i) * exp(1i*self$getParameterValue("location")*t))
})
Gumbel$set("public","mode",function(){
  return(self$getParameterValue("location"))
})
Gumbel$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Gumbel$set("public","initialize",function(location = 0, scale = 1,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, location, scale, verbose)
  self$setParameterValue(list(location = location, scale = scale))

  pdf <- function(x1){
    location <- self$getParameterValue("location")
    scale <- self$getParameterValue("scale")
    z <- (x1 - location)/scale
    return(exp(-(z + exp(-z)))/scale)
  }
  cdf <- function(x1){
    return(exp(-exp(-(x1 - self$getParameterValue("location"))/self$getParameterValue("scale"))))
  }
  quantile <- function(p){
    return(-log(-log(p))*self$getParameterValue("scale") + self$getParameterValue("location"))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(), distrDomain = Reals$new(),
                   symmetric = TRUE)
  invisible(self)
})
