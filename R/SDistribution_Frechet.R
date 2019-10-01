#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Frechet Distribution Documentation
#-------------------------------------------------------------
#' @name Frechet
#' @template SDist
#' @templateVar ClassName Frechet
#' @templateVar DistName Frechet
#' @templateVar uses as a special case of the Generalised Extreme Value distribution
#' @templateVar params shape, \eqn{\alpha}, scale, \eqn{\beta}, and minimum, \eqn{\gamma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)((x-\gamma)/\beta)^{-1-\alpha}exp(-(x-\gamma)/\beta)^{-\alpha}}
#' @templateVar paramsupport \eqn{\alpha, \beta \epsilon R^+} and \eqn{\gamma \epsilon R}
#' @templateVar distsupport \eqn{x > \gamma}
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar aka Inverse Weibull
#' @aliases InverseWeibull
#' @templateVar constructor shape = 1, scale = 1, minimum = 0
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{minimum} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{shape}, \code{scale} as positive numerics and \code{minimum} as a numeric.
#' @templateVar additionalSeeAlso \code{\link{Gumbel}} and \code{\link{Weibull}} for other special cases of the generalized extreme value distribution.
#'
#' @examples
#' x = Frechet$new(shape = 2, scale = 3, minimum = 6)
#'
#' # Update parameters
#' x$setParameterValue(shape = 3)
#' x$parameters()
#'
#' # d/p/q/r
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
Frechet$set("public", "pgf", function(z){
  return(NaN)
})

Frechet$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$minimum)) lst = c(lst, list(minimum = paramlst$minimum))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Frechet$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Interval$new(self$getParameterValue("minimum"),Inf,type="()")
  invisible(self)
})

Frechet$set("public","initialize",function(shape = 1, scale = 1, minimum = 0,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, minimum, verbose)
  self$setParameterValue(shape = shape, scale = scale, minimum = minimum)

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
                   rand = rand, support = Interval$new(minimum, Inf, type = "()"),
                   symmetric = FALSE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Frec", ClassName = "Frechet",
                                                     Type = "\u211D", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))
