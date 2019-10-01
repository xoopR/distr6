#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Normal Distribution Documentation
#-------------------------------------------------------------
#' @name Normal
#' @template SDist
#' @templateVar ClassName Normal
#' @templateVar DistName Normal
#' @templateVar uses in significance testing, for representing models with a bell curve, and as a result of the central limit theorem
#' @templateVar params variance, \eqn{\sigma^2}, and mean, \eqn{\mu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(x-\mu)^2/(2\sigma^2)) / \sqrt{2\pi\sigma^2}}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\sigma^2 > 0}
#' @templateVar distsupport the Reals
#' @templateVar aka Gaussian
#' @aliases Gaussian
#' @templateVar constructor mean = 0, var = 1, sd = NULL, prec = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab mean, location parameter. \cr
#' @templateVar arg2 \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' @templateVar arg3 \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' @templateVar arg4 \code{prec} \tab numeric \tab precision, inverse squared scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric, and either \code{var}, \code{sd} or \code{prec} as numerics. These are related via, \deqn{sd = \sqrt(var)}\deqn{prec = 1/var} If \code{prec} is given then \code{sd} and \code{var} are ignored. If \code{sd} is given then \code{var} is ignored.
#'
#' @examples
#' # Different parameterisations
#' Normal$new(var = 1, mean = 1)
#' Normal$new(prec = 2, mean = 1)
#' Normal$new(mean = 1, sd = 2)
#'
#' x <- Normal$new(verbose = TRUE) # Standard normal default
#'
#' # Update parameters
#' x$setParameterValue(var = 2)
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
# Normal Distribution Definition
#-------------------------------------------------------------
Normal <- R6::R6Class("Normal", inherit = SDistribution, lock_objects = F)
Normal$set("public","name","Normal")
Normal$set("public","short_name","Norm")
Normal$set("public","description","Normal Probability Distribution.")
Normal$set("public","package","stats")

Normal$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Normal$set("public","variance",function(){
  return(self$getParameterValue("var"))
})
Normal$set("public","skewness",function(){
  return(0)
})
Normal$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(0)
  else
    return(3)
})
Normal$set("public","entropy",function(base = 2){
  return(0.5 * log(2 * pi * exp(1) * self$getParameterValue("var"), base))
})
Normal$set("public", "mgf", function(t){
  return(exp((self$getParameterValue("mean") * t) + (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public", "pgf", function(z){
  return(NaN)
})
Normal$set("public", "cf", function(t){
  return(exp((1i * self$getParameterValue("mean") * t) - (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Normal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$var)) lst = c(lst, list(var = paramlst$var))
  if(!is.null(paramlst$sd)) lst = c(lst, list(var = paramlst$sd^2))
  if(!is.null(paramlst$prec)) lst = c(lst, list(var = paramlst$prec^-1))
  return(lst)
})

Normal$set("public","initialize",function(mean = 0, var = 1, sd = NULL, prec = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, var, sd, prec, verbose)
  self$setParameterValue(mean = mean, var = var, sd = sd, prec = prec)

  pdf <- function(x1) dnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  cdf <- function(x1) pnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  quantile <- function(p) qnorm(p, self$getParameterValue("mean"), self$getParameterValue("sd"))
  rand <- function(n) rnorm(n, self$getParameterValue("mean"), self$getParameterValue("sd"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T),
                   symmetric = TRUE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
