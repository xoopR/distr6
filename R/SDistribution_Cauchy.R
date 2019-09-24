#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Cauchy Distribution Documentation
#-------------------------------------------------------------
#' @name Cauchy
#' @author Chijing Zeng
#' @template SDist
#' @templateVar ClassName Cauchy
#' @templateVar DistName Cauchy
#' @templateVar uses in physics and finance
#' @templateVar params location, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1 / (\pi\beta(1 + ((x - \alpha) / \beta)^2))}
#' @templateVar paramsupport \eqn{\alpha \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar additionalDetails The mean and variance are undefined, hence \code{NaN} is returned.
#' @templateVar constructor location = 0, scale = 1
#' @templateVar arg1 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{location} as a numeric and \code{scale} as a positive numeric.
#'
#' @examples
#' x = Cauchy$new(location = 2, scale = 5)
#'
#' # Update parameters
#' x$setParameterValue(scale = 3)
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
# Cauchy Distribution Definition
#-------------------------------------------------------------
Cauchy <- R6::R6Class("Cauchy", inherit = SDistribution, lock_objects = F)
Cauchy$set("public","name","Cauchy")
Cauchy$set("public","short_name","Cauchy")
Cauchy$set("public","description","Cauchy Probability Distribution.")
Cauchy$set("public","package","stats")

Cauchy$set("public","mean",function(){
  return(NaN)
})
Cauchy$set("public","variance",function(){
  return(NaN)
})
Cauchy$set("public","skewness",function(){
  return(NaN)
})
Cauchy$set("public","kurtosis",function(excess = TRUE){
  return(NaN)
})
Cauchy$set("public","entropy",function(base = 2){
  return(log(4 * pi * self$getParameterValue("scale"), base))
})
Cauchy$set("public", "mgf", function(t){
  return(NaN)
})
Cauchy$set("public", "pgf", function(z){
  return(NaN)
})
Cauchy$set("public", "cf", function(t){
  return(exp((self$getParameterValue("location") * 1i * t) - (self$getParameterValue("scale") * abs(t))))
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
  self$setParameterValue(location = location, scale = scale)

  pdf <- function(x1) dcauchy(x1, self$getParameterValue("location"), self$getParameterValue("scale"))
  cdf <- function(x1) pcauchy(x1, self$getParameterValue("location"), self$getParameterValue("scale"))
  quantile <- function(p) qcauchy(p, self$getParameterValue("location"), self$getParameterValue("scale"))
  rand <- function(n) rcauchy(n, self$getParameterValue("location"), self$getParameterValue("scale"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(),
                   symmetric = TRUE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Cauchy", ClassName = "Cauchy",
                                                     Type = "\u211D", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))

