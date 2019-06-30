#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Pareto Distribution Documentation
#-------------------------------------------------------------
#' @title Pareto Distribution
#' @description Mathematical and statistical functions for the Pareto distribution parameterised
#' with shape and scale and defined by the pdf,
#' \deqn{f(x) = (\alpha * \beta^\alpha)/(x^(\alpha+1))}
#' where \eqn{\alpha > 0} is the shape parameter and \eqn{\beta > 0} is the scale parameter.
#'
#' @details \code{cf} is omitted as no analytic expression involving the incomplete gamma function
#' with complex numbers could be found.
#'
#' @name Pareto
#'
#' @section Constructor: Pareto$new(shape = 1, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Pareto distribution is parameterised with shape and scale parameters.
#' Default parameterisation is with shape = 1 and scale = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Pareto$new(shape = 2, scale = 1)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 5.1))
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
# Pareto Distribution Definition
#-------------------------------------------------------------
Pareto <- R6::R6Class("Pareto", inherit = SDistribution, lock_objects = F)
Pareto$set("public","name","Pareto")
Pareto$set("public","short_name","Pare")
Pareto$set("public","traits",list(type = PosReals$new(zero = T),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))
Pareto$set("public","description","Pareto Probability Distribution.")
Pareto$set("public","package","distr6")

Pareto$set("public","mean",function(){
  if(self$getParameterValue("shape") <= 1)
    return(Inf)
  else
    return((self$getParameterValue("shape") * self$getParameterValue("scale"))/(self$getParameterValue("shape")-1))
})
Pareto$set("public","var",function(){
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")
  if(shape <= 2)
    return(Inf)
  else
    return((shape*scale^2)/((shape-1)^2 * (shape-2)))
})
Pareto$set("public","skewness",function(){
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")
  if(shape > 3){
    return(((2*(1+shape))/(shape-3)) * sqrt((shape-2)/shape))
  } else
    return(NaN)
})
Pareto$set("public","kurtosis",function(excess = TRUE){
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")
  if(shape > 4){
    kur = (6 * (shape^3 + shape^2 - 6*shape - 2))/(shape*(shape-3)*(shape-4))
  } else
    return(NaN)

  if(excess)
    return(kur)
  else
    return(kur + 3)
})
Pareto$set("public","entropy",function(base = 2){
  shape <- self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")

  return(log((scale/shape) * exp(1 + 1/shape), base))
})
Pareto$set("public", "mgf", function(t){
  if(t < 0){
    shape <- self$getParameterValue("shape")
    scale <- self$getParameterValue("scale")
    return(shape * (-scale*t)^shape * expint::gammainc(-shape, -scale * t))
  }else
    return(NaN)
})
Pareto$set("public","mode",function(){
  return(self$getParameterValue("scale"))
})

Pareto$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  private$.properties$support <- Interval$new(self$getParameterValue("scale"), Inf, type = "[)")
})

Pareto$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Pareto$set("public","initialize",function(shape = 1, scale = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, verbose)
  self$setParameterValue(list(shape = shape, scale = scale))

  pdf <- function(x1){
    shape <- self$getParameterValue("shape")
    scale <- self$getParameterValue("scale")
    return((shape * scale^shape)/(x1^(shape+1)))
  }
  cdf <- function(x1){
    return(1 - ((self$getParameterValue("scale")/x1)^self$getParameterValue("shape")))
  }
  quantile <- function(p){
    return(self$getParameterValue("scale") * (1-p)^(-1/self$getParameterValue("shape")))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(scale, Inf, type = "[)"), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})
