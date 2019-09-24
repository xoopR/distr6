#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Laplace Distribution Documentation
#-------------------------------------------------------------
#' @name Laplace
#' @template SDist
#' @templateVar ClassName Laplace
#' @templateVar DistName Laplace
#' @templateVar uses in signal processing and finance
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-|x-\mu|/\beta)/(2\beta)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor mean = 0, scale = 1, var = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{var} \tab numeric \tab alternate scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric and either \code{scale} or \code{var} as positive numerics. These are related via, \deqn{var = 2 * scale^2} If \code{var} is given then {scale} is ignored.
#'
#' @examples
#' Laplace$new(scale = 2)
#' Laplace$new(var = 4)
#'
#' x = Laplace$new(verbose = TRUE) # Default is mean = 0, scale = 1
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
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
# Laplace Distribution Definition
#-------------------------------------------------------------
Laplace <- R6::R6Class("Laplace", inherit = SDistribution, lock_objects = F)
Laplace$set("public","name","Laplace")
Laplace$set("public","short_name","Lap")
Laplace$set("public","description","Laplace Probability Distribution.")
Laplace$set("public","package","distr6")

Laplace$set("public","mean",function(){
  self$getParameterValue("mean")
})
Laplace$set("public","variance",function(){
  self$getParameterValue("var")
})
Laplace$set("public","skewness",function(){
  return(0)
})
Laplace$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(3)
  else
    return(6)
})
Laplace$set("public","entropy",function(base = 2){
  return(log(2 * exp(1) * self$getParameterValue("scale"), base))
})
Laplace$set("public", "mgf", function(t){
  if(abs(t) < 1/self$getParameterValue("scale"))
    return(exp(self$getParameterValue("mean") * t) / (1 - self$getParameterValue("scale")^2 * t^2))
  else
    return(NaN)
})
Laplace$set("public", "pgf", function(z){
  return(NaN)
})
Laplace$set("public", "cf", function(t){
  return(exp(self$getParameterValue("mean") * t * 1i) / (1 + self$getParameterValue("scale")^2 * t^2))
})
Laplace$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Laplace$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  if(!is.null(paramlst$var)) lst = c(lst, list(scale = sqrt(paramlst$var/2)))
  return(lst)
})

Laplace$set("public","initialize",function(mean = 0, scale = 1, var = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, scale, var, verbose)
  self$setParameterValue(mean = mean, scale = scale, var = var)

  pdf <- function(x1){
    return((2*self$getParameterValue("scale"))^-1 * exp(-abs(x1-self$getParameterValue("mean"))))
  }
  cdf <- function(x1){
    cdf = x1
    cdf[x1 <= self$getParameterValue("mean")] = 0.5 * exp((cdf[x1 <= self$getParameterValue("mean")]-
                                                             self$getParameterValue("mean"))/
                                                            self$getParameterValue("scale"))
    cdf[x1 > self$getParameterValue("mean")] = 1 - (0.5 * exp(-(cdf[x1 > self$getParameterValue("mean")]-
                                                                  self$getParameterValue("mean"))/
                                                                self$getParameterValue("scale")))
    return(cdf)
  }
  quantile <- function(p){
    quantile = p
    quantile[p <= 0.5] = self$getParameterValue("mean") + (self$getParameterValue("scale")*
                                                             log(2*quantile[p <= 0.5]))
    quantile[p > 0.5] = self$getParameterValue("mean") - (self$getParameterValue("scale")*
                                                            log(2 - 2*quantile[p > 0.5]))
    return(quantile)
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Reals$new(),
                   symmetric = TRUE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Lap", ClassName = "Laplace",
                                                     Type = "\u211D", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))
