#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Student's t Distribution Documentation
#-------------------------------------------------------------
#' @name StudentT
#' @author Chijing Zeng
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
#' @templateVar constructorDets \code{df} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{StudentTNoncentral}} for the noncentral Student's T distribution.
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
    return(0)
  else
    return(NaN)
})
StudentT$set("public","variance",function(){
  df <- self$getParameterValue("df")
  if(df > 2)
    return(df/(df-2))
  else
    return(NaN)
})
StudentT$set("public","skewness",function(){
  if(self$getParameterValue("df") > 3)
    return(0)
  else
    return(NaN)
})
StudentT$set("public","kurtosis",function(excess = TRUE){
  df <- self$getParameterValue("df")
  if(df > 4)
    exkurtosis = 6/(df-4)
  else
    return(NaN)

  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
StudentT$set("public","entropy",function(base = 2){
  df <- self$getParameterValue("df")
  return((((df+1)/2)*(digamma((1+df)/2) - digamma(df/2))) + (log(sqrt(df)*beta(df/2, 1/2), base)))
})
StudentT$set("public", "mgf", function(t){
  return(NaN)
})
StudentT$set("public", "cf", function(t){
  df <- self$getParameterValue("df")
  return((besselK(sqrt(df)*abs(t), df/2) * ((sqrt(df)*abs(t))^(df/2))) / (gamma(df/2)*2^(df/2-1)))
})
StudentT$set("public", "pgf", function(z){
  return(NaN)
})
StudentT$set("public","mode",function(){
  return(0)
})

StudentT$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  return(lst)
})

StudentT$set("public","initialize",function(df = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df, verbose)
  self$setParameterValue(df = df)

  pdf <- function(x1) dt(x1, self$getParameterValue("df"))
  cdf <- function(x1) pt(x1, self$getParameterValue("df"))
  quantile <- function(p) qt(p, self$getParameterValue("df"))
  rand <- function(n) rt(n, self$getParameterValue("df"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T),
                   symmetric  = TRUE,type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
