#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Student's t Distribution Documentation
#-------------------------------------------------------------
#' @name StudentT
#' @template SDist
#' @templateVar ClassName StudentT
#' @templateVar DistName Student's T
#' @templateVar uses to estimate the mean of populations with unknown variance from a small sample size, as well as in t-testing for difference of means and regression analysis
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \Gamma((\nu+1)/2)/(\sqrt(\nu\pi)\Gamma(\nu/2)) * (1+(x^2)/\nu)^(-(\nu+1)/2)}
#' @templateVar paramsupport \eqn{\nu > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor df = 1
#' @templateVar arg1 \code{df} \tab numeric \tab degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab non-centrality parameter (ncp in rstats). \cr
#' @templateVar constructorDets \code{df} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution.
#'
#' @examples
#' x = StudentT$new(df = 2)
#'
#' # Update parameters
#' x$setParameterValue(df = 3)
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
# Student's t Distribution Definition
#-------------------------------------------------------------
StudentT <- R6::R6Class("StudentT", inherit = SDistribution, lock_objects = F)
StudentT$set("public","name","StudentT")
StudentT$set("public","short_name","T")
StudentT$set("public","description","Student's t Probability Distribution.")
StudentT$set("public","package","stats")

StudentT$set("public", "mean", function(){
  df <- self$getParameterValue("df")
  if (df > 1)
    return(self$getParameterValue("location")*sqrt(df/2)*gamma((df - 1)/2)/gamma(df/2))
  else
    return(NaN)
})
StudentT$set("public","variance",function(){
  df <- self$getParameterValue("df")
  mu <- self$getParameterValue("location")
  if(df > 2)
    return(df*(1 + mu^2)/(df-2) - (mu^2*df/2)*(gamma((df - 1)/2)/gamma(df/2))^2)
  else
    return(NaN)
})
StudentT$set("public","skewness",function(){
  if(self$getParameterValue("df") > 3 & self$getParameterValue("location") == 0)
    return(0)
  else
    return(NaN)
})
StudentT$set("public","kurtosis",function(excess = TRUE){
  if (self$getParameterValue("location") == 0){
    df <- self$getParameterValue("df")
    if(df > 4)
      exkurtosis = 6/(df-4)
    else
      return(NaN)
    
    if(excess)
      return(exkurtosis)
    else
      return(exkurtosis + 3)
  }
  else
    return(NaN)
})
StudentT$set("public","entropy",function(base = 2){
  df <- self$getParameterValue("df")
  if (self$getParameterValue("location") == 0)
    return((((df+1)/2)*(digamma((1+df)/2) - digamma(df/2))) + (log(sqrt(df)*beta(df/2, 1/2), base)))
  else
    return(NaN)
})
StudentT$set("public", "mgf", function(t){
  return(NaN)
})
StudentT$set("public", "cf", function(t){
  df <- self$getParameterValue("df")
  if (self$getParameterValue("location") == 0)
    return((besselK(sqrt(df)*abs(t), df/2) * ((sqrt(df)*abs(t))^(df/2))) / (gamma(df/2)*2^(df/2-1)))
  else
    return(NaN)
})
StudentT$set("public", "pgf", function(z){
  return(NaN)
})
StudentT$set("public","mode",function(){
  if (self$getParameterValue("location") == 0)
    return(0)
  else
    return(NaN)
})

StudentT$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})

StudentT$set("public","initialize",function(df = 1, location = 0, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df, location, verbose)
  self$setParameterValue(df = df, location = location)

  pdf <- function(x1) dt(x1, self$getParameterValue("df"), self$getParameterValue("location"))
  cdf <- function(x1) pt(x1, self$getParameterValue("df"), self$getParameterValue("location"))
  quantile <- function(p) qt(p, self$getParameterValue("df"), self$getParameterValue("location"))
  rand <- function(n) rt(n, self$getParameterValue("df"), self$getParameterValue("location"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T),
                   symmetric  = TRUE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
