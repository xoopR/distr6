#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Beta Distribution Documentation
#-------------------------------------------------------------
#' @name Beta
#' @template SDist
#' @templateVar ClassName Beta
#' @templateVar DistName Beta
#' @templateVar uses as the prior in Bayesian modelling
#' @templateVar params two shape parameters, \eqn{\alpha, \beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\alpha-1}(1-x)^{\beta-1}) / B(\alpha, \beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{B} is the Beta function
#' @templateVar distsupport \eqn{[0, 1]}
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar constructor shape1 = 1, shape2 = 1
#' @templateVar arg1 \code{shape1, shape2} \tab numeric \tab positive shape parameter. \cr
#' @templateVar constructorDets  \code{shape1} and \code{shape2} as positive numerics.
#'
#' @examples
#' x = Beta$new(shape1 = 2, shape2 = 5)
#'
#' # Update parameters
#' x$setParameterValue(shape1 = 1)
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
# Beta Distribution Definition
#-------------------------------------------------------------
Beta <- R6::R6Class("Beta", inherit = SDistribution, lock_objects = F)
Beta$set("public","name","Beta")
Beta$set("public","short_name","Beta")
Beta$set("public","description","Beta Probability Distribution.")
Beta$set("public","package","stats")


Beta$set("public","mean",function(){
  return(self$getParameterValue("shape1") / (self$getParameterValue("shape1") + self$getParameterValue("shape2")))
})
Beta$set("public","variance",function(){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  return(shape1*shape2*((shape1+shape2)^-2)*(shape1+shape2+1)^-1)
})
Beta$set("public","mode",function(which = "all"){
  if(self$getParameterValue("shape1")<=1 & self$getParameterValue("shape2")>1)
    return(0)
  else if(self$getParameterValue("shape1")>1 & self$getParameterValue("shape2")<=1)
    return(1)
  else if(self$getParameterValue("shape1")<1 & self$getParameterValue("shape2")<1){
    if(which == "all")
      return(c(0,1))
    else
      return(c(0,1)[which])
  } else if(self$getParameterValue("shape1")>1 & self$getParameterValue("shape2")>1)
    return((self$getParameterValue("shape1")-1)/(self$getParameterValue("shape1")+self$getParameterValue("shape2")-2))
})
Beta$set("public","skewness",function(){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  return(2*(shape2-shape1)*((shape1+shape2+1)^0.5)*((shape1+shape2+2)^-1)*((shape1*shape2)^-0.5))
})
Beta$set("public","kurtosis",function(excess = TRUE){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")

  ex_kurtosis = 6*{((shape1-shape2)^2)*(shape1+shape2+1)-(shape1*shape2*(shape1+shape2+2))}/
    (shape1*shape2*(shape1+shape2+2)*(shape1+shape2+3))
  if (excess)
    return(ex_kurtosis)
  else
    return(ex_kurtosis+3)
})
Beta$set("public", "entropy", function(base = 2){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  return(log(beta(shape1,shape2), base) - ((shape1-1)*digamma(shape1)) -
           ((shape2-1) * digamma(shape2)) + ((shape1+shape2-2)*digamma(shape1+shape2)))
})
Beta$set("public", "pgf", function(z){
  return(NaN)
})

Beta$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape1)) lst = c(lst,list(shape1 = paramlst$shape1))
  if(!is.null(paramlst$shape2)) lst = c(lst,list(shape2 = paramlst$shape2))
  return(lst)
})

Beta$set("public", "initialize", function(shape1 = 1, shape2 = 1, decorators = NULL,verbose = FALSE){

  private$.parameters <- getParameterSet.Beta(self, shape1, shape2, verbose)
  self$setParameterValue(shape1=shape1,shape2=shape2)

  pdf <- function(x1) dbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  cdf <- function(x1) pbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  quantile <- function(p) qbeta(p, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  rand <- function(n) rbeta(n, self$getParameterValue("shape1"), self$getParameterValue("shape2"))


  if (shape1 == shape2)
    symmetric <- TRUE
  else
    symmetric <- FALSE

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(0,1),
                   symmetric = symmetric, type = PosReals$new(zero = T),
                   valueSupport ="continuous",
                   variateForm = "univariate")

  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Beta", ClassName = "Beta",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
