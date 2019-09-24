#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gamma Distribution Documentation
#-------------------------------------------------------------
#' @name Gamma
#' @template SDist
#' @templateVar ClassName Gamma
#' @templateVar DistName Gamma
#' @templateVar uses as the prior in Bayesian modelling, the convolution of exponential distributions, and to model waiting times
#' @templateVar params shape, \eqn{\alpha}, and rate, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha)x^{\alpha-1}exp(-x\beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor shape = 1, rate = 1, scale = NULL, mean = NULL
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{rate} \tab numeric \tab inverse scale parameter. \cr
#' @templateVar arg3 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg4 \code{mean} \tab numeric \tab alternate scale parameter. \cr
#' @templateVar constructorDets \code{shape} and either \code{rate}, \code{scale} or \code{mean}, all as positive numerics. These are related via, \deqn{scale = 1/rate} \deqn{mean = shape/rate} If \code{mean} is given then \code{rate} and \code{scale} are ignored. If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Gamma$new(shape = 1, rate = 2)
#' Gamma$new(shape = 1, scale = 4)
#' Gamma$new(shape = 1, mean = 0.5)
#'
#' # Default is shape = 1, rate = 1
#' x = Gamma$new(verbose = TRUE)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(scale = 2)
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
# Gamma Distribution Definition
#-------------------------------------------------------------
Gamma <- R6::R6Class("Gamma", inherit = SDistribution, lock_objects = F)
Gamma$set("public","name","Gamma")
Gamma$set("public","short_name","Gamma")
Gamma$set("public","description","Gamma Probability Distribution.")
Gamma$set("public","package","stats")

Gamma$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Gamma$set("public","variance",function(){
  return(self$getParameterValue("mean")*self$getParameterValue("scale"))
})
Gamma$set("public","skewness",function() {
  2/sqrt(self$getParameterValue("shape"))
})
Gamma$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6/self$getParameterValue("shape"))
  else
    return((6/self$getParameterValue("shape"))+3)
})
Gamma$set("public","entropy",function(base = 2){
  self$getParameterValue("shape") - log(self$getParameterValue("rate"), base) +
    log(gamma(self$getParameterValue("shape")), base) + (1-self$getParameterValue("shape"))*digamma(self$getParameterValue("shape"))
})
Gamma$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return((1-self$getParameterValue("scale")*t)^(-self$getParameterValue("shape")))
  else
    return(NaN)
})
Gamma$set("public", "pgf", function(z){
  return(NaN)
})
Gamma$set("public", "cf", function(t){
  return((1-self$getParameterValue("scale")*1i*t)^(-self$getParameterValue("shape"))   )
})
Gamma$set("public","mode",function(){
  if(self$getParameterValue("shape")>=1)
    return((self$getParameterValue("shape")-1)/self$getParameterValue("rate"))
  else
    return(NaN)
})

Gamma$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))
  if(!is.null(paramlst$mean)){
    if(is.null(paramlst$shape))
      lst = c(lst, list(rate = self$getParameterValue("shape")/paramlst$mean))
    else
      lst = c(lst, list(rate = paramlst$shape/paramlst$mean))
  }

  return(lst)
})

Gamma$set("public","initialize",function(shape = 1,rate = 1, scale = NULL, mean = NULL, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.Gamma(self, shape, rate, scale, mean, verbose)
  self$setParameterValue(shape = shape, rate = rate, scale = scale, mean = mean)

  pdf <- function(x1) dgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  cdf <- function(x1) pgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  quantile <- function(p) qgamma(p, self$getParameterValue("shape"),self$getParameterValue('rate'))
  rand <- function(n) rgamma(n, self$getParameterValue("shape"),self$getParameterValue('rate'))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = F),
                   symmetric  = FALSE,type = PosReals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Gamma", ClassName = "Gamma",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
