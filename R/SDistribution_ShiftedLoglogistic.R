
#-------------------------------------------------------------
# Log-Logistic Distribution Documentation
#-------------------------------------------------------------
#' @name ShiftedLoglogistic
#' @template SDist
#' @templateVar ClassName ShiftedLoglogistic
#' @templateVar DistName Shifted Log-Logistic
#' @templateVar uses in survival analysis for its non-monotonic hazard as well as in economics, a generalised variant of [Loglogistic]
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
#' @templateVar additionalSeeAlso [Logistic], [Loglogistic].
#'
#' @examples
#' x <- ShiftedLoglogistic$new(shape = 2, scale = 3, location = 2)
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
# ShiftedLoglogistic Distribution Definition
#-------------------------------------------------------------
ShiftedLoglogistic <- R6Class("ShiftedLoglogistic", inherit = SDistribution, lock_objects = F)
ShiftedLoglogistic$set("public","name","ShiftedLoglogistic")
ShiftedLoglogistic$set("public","short_name","ShiftLLogis")
ShiftedLoglogistic$set("public","description","Shifted Log-Logistic Probability Distribution.")
ShiftedLoglogistic$set("public","packages","pracma")

ShiftedLoglogistic$set("public","mean",function(){
  return(self$getParameterValue("location") +
           (self$getParameterValue("scale")*pi/self$getParameterValue("shape"))/
           sin(pi/self$getParameterValue("shape")))
})
ShiftedLoglogistic$set("public","variance",function(){
  if(self$getParameterValue("shape") > 2){
    scale <- self$getParameterValue("scale")
    shapi <- pi/self$getParameterValue("shape")
    return(scale^2 * ((2*shapi)/sin(2*shapi) - (shapi^2)/sin(shapi)^2))
  } else
    return(NaN)
})
ShiftedLoglogistic$set("public","skewness",function(){
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
ShiftedLoglogistic$set("public","kurtosis",function(excess = TRUE){
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
ShiftedLoglogistic$set("public","mode",function(which = NULL){
  shape <- self$getParameterValue("shape")
  return(self$getParameterValue("location") +
           self$getParameterValue("scale")*((shape-1)/(shape+1))^(1/shape))
})
ShiftedLoglogistic$set("public", "pgf", function(z){
  return(NaN)
})

ShiftedLoglogistic$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Interval$new(self$getParameterValue("location"),Inf,type="()")
  invisible(self)
})

ShiftedLoglogistic$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale))
  if(!is.null(paramlst$rate)) lst = c(lst, list(scale = paramlst$rate^-1))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  return(lst)
})
ShiftedLoglogistic$set("private",".pdf", function(x){
  location <- self$getParameterValue("location")
  shape <-self$getParameterValue("shape")
  scale <- self$getParameterValue("scale")

  return((shape/scale) * (((x - location)/scale)^(shape-1)) * (1 + ((x - location)/scale)^shape)^-2)
})
ShiftedLoglogistic$set("private",".cdf", function(x){
  (1 + ((x - self$getParameterValue("location"))/self$getParameterValue("scale"))^-self$getParameterValue("shape"))^-1
})
ShiftedLoglogistic$set("private",".quantile", function(p){
  self$getParameterValue("scale")*(p/(1-p))^(1/self$getParameterValue("shape")) + self$getParameterValue("location")
})
ShiftedLoglogistic$set("private",".rand", function(n){
  self$getParameterValue("location") + self$getParameterValue("scale") * pracma::nthroot(runif(n)/(1-runif(n)), self$getParameterValue("shape"))
})
ShiftedLoglogistic$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

ShiftedLoglogistic$set("public","initialize",function(scale = 1, shape = 1, location = 0,
                                               rate = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, scale, shape, location, rate, verbose)
  self$setParameterValue(scale = scale, shape = shape, location = location, rate = rate)

  super$initialize(decorators = decorators,
                   support = Interval$new(location,Inf,type="()"),
                   type = PosReals$new(zero = T))
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "ShiftLLogis", ClassName = "ShiftedLoglogistic",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "-"))
