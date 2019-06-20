#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Cauchy Distribution Documentation
#-------------------------------------------------------------
#' @title Cauchy Distribution
#'
#' @description Mathematical and statistical functions for the Cauchy distribution parameterised
#' with location and scale.
#' The location/scale parameterisation is defined by the pdf,
#' \deqn{f(x) = 1 / (\pi * \gamma * (1 + ((x - \x0) / \gamma)^2))}
#' where \eqn{\x0} is the location parameter and \eqn{\gamma} > 0 is the scale parameter.
#'
#' @name Cauchy
#'
#' @section Constructor: Cauchy$new(location = 0, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{location} \tab numeric \tab location, location parameter. \cr
#' \code{scale} \tab numeric \tab scale, scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Cauchyt distribution is parameterised with
#' location and scale. Default parameterisation is with location = 0 and scale = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @export
NULL
#-------------------------------------------------------------
# Cauchy Distribution Definition
#-------------------------------------------------------------
Cauchy <- R6::R6Class("Cauchy", inherit = SDistribution, lock_objects = F)
Cauchy$set("public","name","Cauchy")
Cauchy$set("public","short_name","Cauchy")
Cauchy$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Cauchy$set("public","description","Cauchy Probability Distribution.")

Cauchy$set("public","mean",function(){
  return(NaN)
})
Cauchy$set("public","var",function(){
  return(NaN)
})
Cauchy$set("public","skewness",function(){
  return(NaN)
})
Cauchy$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(NaN)
  else
    return(NaN)
})
Cauchy$set("public","entropy",function(base = 2){
  return(log(4 * pi * self$getParameterValue("scale"), base))
})
Cauchy$set("public", "mgf", function(t){
  return(NaN)
})
Cauchy$set("public", "cf", function(t){
  return(exp((self$getParameterValue("location") * i * t) - (self$getParameterValue("scale") * abs(t))))
})
Cauchy$set("public","mode",function(){
  return(self$getParameterValue("location"))
})

Cauchy$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Cauchy$set("public","initialize",function(location = 0, scale = 1,
                                          decorators = NULL, verbose = FALSE){
  
  private$.parameters <- getParameterSet(self, location, scale, verbose)
  self$setParameterValue(list(location = location, scale = scale))
  
  pdf <- function(x1) dcauchy(x1, self$getParameterValue("location"), self$getParameterValue("scale"))
  cdf <- function(x1) pcauchy(x1, self$getParameterValue("location"), self$getParameterValue("scale"))
  quantile <- function(p) qcauchy(p, self$getParameterValue("location"), self$getParameterValue("scale"))
  rand <- function(n) rcauchy(n, self$getParameterValue("location"), self$getParameterValue("scale"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})