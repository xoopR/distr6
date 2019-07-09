#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Pareto Distribution Documentation
#-------------------------------------------------------------
#' @title Pareto Distribution Class
#'
#' @description Mathematical and statistical functions for the Pareto distribution, which is commonly
#' used in Economics to model the distribution of wealth and the 80-20 rule.
#'
#' @details The Pareto distribution parameterised with shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' is defined by the pdf,
#' \deqn{f(x) = (\alpha * \beta^\alpha)/(x^(\alpha+1))}
#' for \eqn{\alpha > 0} and \eqn{\beta > 0}.
#'
#' The distribution is supported on \eqn{[\beta, \infty)}.
#'
#' \code{cf} is omitted as no analytical expression involving the incomplete gamma function
#' with complex numbers could be found. Decorate with \code{CoreStatistics} for numerical results.
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
#' @section Constructor Details: The Pareto distribution is parameterised with \code{shape} and \code{scale}
#' as positive numerics.
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
#' x$variance()
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
Pareto$set("public","description","Pareto Probability Distribution.")
Pareto$set("public","package","distr6")

Pareto$set("public","mean",function(){
  if(self$getParameterValue("shape") <= 1)
    return(Inf)
  else
    return((self$getParameterValue("shape") * self$getParameterValue("scale"))/(self$getParameterValue("shape")-1))
})
Pareto$set("public","variance",function(){
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
                   symmetric  = FALSE,type = PosReals$new(zero = T),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
