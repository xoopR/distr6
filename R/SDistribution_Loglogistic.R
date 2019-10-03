#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Log-Logistic Distribution Documentation
#-------------------------------------------------------------
#' @name Loglogistic
#' @template SDist
#' @templateVar ClassName Loglogistic
#' @templateVar DistName Log-Logistic
#' @templateVar uses in survival analysis for its non-monotonic hazard as well as in economics
#' @templateVar params shape, \eqn{\beta}, scale, \eqn{\alpha}, and location, \eqn{\gamma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta/\alpha)((x-\gamma)/\alpha)^{\beta-1}(1 + ((x-\gamma)/\alpha)^\beta)^{-2}}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0} and \eqn{\gamma >= 0}
#' @templateVar distsupport the non-negative Reals
#' @templateVar omittedVars \code{entropy}, \code{mgf} and \code{cf}
#' @templateVar aka Fisk
#' @aliases Fisk
#' @templateVar constructor scale = 1, shape = 1, location = 0
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics and \code{location} as a numeric.
#' @templateVar additionalSeeAlso \code{\link{Logistic}} for the Logistic distribution.
#'
#' @examples
#' x <- Loglogistic$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(scale = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5:6)
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
# Loglogistic Distribution Definition
#-------------------------------------------------------------
Loglogistic <- R6::R6Class("Loglogistic", inherit = SDistribution, lock_objects = F)
Loglogistic$set("public","name","Loglogistic")
Loglogistic$set("public","short_name","LLogis")
Loglogistic$set("public","description","Log-Logistic Probability Distribution.")
Loglogistic$set("public","package","distr6")

Loglogistic$set("public","mean",function(){
  return(self$getParameterValue("location") +
           (self$getParameterValue("scale")*pi/self$getParameterValue("shape"))/
           sin(pi/self$getParameterValue("shape")))
})
Loglogistic$set("public","variance",function(){
  if(self$getParameterValue("shape") > 2){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    return(scale^2 * ((2*shapi)/sin(2*shapi) - (shapi^2)/sin(shapi)^2))
  } else
    return(NaN)
})
Loglogistic$set("public","skewness",function(){
  if(self$getParameterValue("shape") > 3){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    s1 <- (2*shapi^3*scale^3)/sin(shapi)^3
    s2 <- (6*shapi^2*scale^3)*(1/sin(shapi))*(1/sin(2*shapi))
    s3 <- (3*shapi*scale^3)/sin(3*shapi)
    return(s1-s2+s3)
  } else
    return(NaN)
})
Loglogistic$set("public","kurtosis",function(excess = TRUE){
  if(self$getParameterValue("shape") > 4){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    s1 <- (3*shapi^4*scale^4)/sin(shapi)^4
    s2 <- (12*shapi^3*scale^4)*(1/sin(shapi)^2)*(1/sin(2*shapi))
    s3 <- (12*shapi^2*scale^4)*(1/sin(shapi))*(1/sin(3*shapi))
    s4 <- (4*shapi*scale^4)*(1/sin(4*shapi))
    kurtosis = -s1 + s2 - s3 + s4
    if(excess)
      return(kurtosis - 3)
    else
      return(kurtosis)
  } else
    return(NaN)
})
Loglogistic$set("public","mode",function(){
  shape <- self$getParameterValue("shape")
  return(self$getParameterValue("location") +
           self$getParameterValue("scale")*((shape-1)/(shape+1))^(1/shape))
})
Loglogistic$set("public", "pgf", function(z){
  return(NaN)
})

Loglogistic$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Interval$new(self$getParameterValue("location"),Inf,type="()")
  invisible(self)
})

Loglogistic$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  return(lst)
})

Loglogistic$set("public","initialize",function(scale = 1, shape = 1, location = 0,
                                               decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, scale, shape, location, verbose)
  self$setParameterValue(scale = scale, shape = shape, location = location)

  pdf <- function(x1){
    location <- self$getParameterValue("location")
    shape <- self$getParameterValue("shape")
    scale <- self$getParameterValue("scale")

    return((shape/scale) * (((x1 - location)/scale)^(shape-1)) * (1 + ((x1 - location)/scale)^shape)^-2)
  }
  cdf <- function(x1){
    return((1 + ((x1 -self$getParameterValue("location"))/self$getParameterValue("scale"))^-self$getParameterValue("shape"))^-1)
  }
  quantile <- function(p){
    return(self$getParameterValue("scale")*(p/(1-p))^(1/self$getParameterValue("shape")) + self$getParameterValue("location"))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(location,Inf,type="()"),
                   symmetric = FALSE,type = PosReals$new(zero = T),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "LLogis", ClassName = "Loglogistic",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))
