#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Chi-Squared Distribution Documentation
#-------------------------------------------------------------
#' @name ChiSquared
#' @template SDist
#' @templateVar ClassName ChiSquared
#' @templateVar DistName Chi-Squared
#' @templateVar uses to model the sum of independent squared Normal distributions and for confidence intervals
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\nu/2-1} exp(-x/2))/(2^{\nu/2}\Gamma(\nu/2))}
#' @templateVar paramsupport \eqn{\nu > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor df = 1
#' @templateVar arg1 \code{df} \tab numeric \tab  degrees of freedom. \cr
#' @templateVar constructorDets \code{df} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{ChiSquaredNoncentral}} for the noncentral Chi-Squared distribution.
#'
#' @examples
#' x = ChiSquared$new(df = 2)
#'
#' # Update parameters
#' x$setParameterValue(location = 3)
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
# ChiSquared Distribution Definition
#-------------------------------------------------------------
ChiSquared <- R6::R6Class("ChiSquared", inherit = SDistribution, lock_objects = FALSE)
ChiSquared$set("public", "name", "ChiSquared")
ChiSquared$set("public", "short_name", "ChiSq")
ChiSquared$set("public", "description", "ChiSquared Probability Distribution")
ChiSquared$set("public","package","stats")

ChiSquared$set("public", "mean", function(){
  return(self$getParameterValue("df"))
})
ChiSquared$set("public", "variance", function(){
  return(self$getParameterValue("df")*2)
})
ChiSquared$set("public", "skewness", function(){
  return(sqrt(8/self$getParameterValue("df")))
})
ChiSquared$set("public", "kurtosis", function(excess = TRUE){
  if(excess)
    return(12/self$getParameterValue("df"))
  else
    return(12/self$getParameterValue("df") + 3)
})
ChiSquared$set("public", "entropy", function(base = 2){
  return(self$getParameterValue("df")/2 + log(2*gamma(self$getParameterValue("df")/2), base) +
           ((1 - self$getParameterValue("df")/2)*digamma(self$getParameterValue("df")/2)))
})
ChiSquared$set("public", "mgf", function(t){
  if(t < 0.5){
    return((1 - 2*t)^(-self$getParameterValue("df")/2))
  } else{
    return(NaN)
  }
})
ChiSquared$set("public", "cf", function(t){
  return((1 - 2i*t)^(-self$getParameterValue("df")/2))
})
ChiSquared$set("public", "pgf", function(z){
  if(z > 0 & z < sqrt(exp(1)))
    return((1 - 2 * log(z))^(-self$getParameterValue("df")/2))
  else
    return(NaN)
})
ChiSquared$set("public", "mode", function(){
  return(max(self$getParameterValue("df") - 2, 0))
})

ChiSquared$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  if(self$getParameterValue("df") == 1)
    private$.properties$support <- PosReals$new(zero = F)
  else
    private$.properties$support <- PosReals$new(zero = T)
  invisible(self)
})
ChiSquared$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  return(lst)
})

ChiSquared$set("public","initialize",function(df = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df, verbose)
  self$setParameterValue(df = df)

  pdf <- function(x1) dchisq(x1, self$getParameterValue("df"))
  cdf <- function(x1) pchisq(x1, self$getParameterValue("df"))
  quantile <- function(p) qchisq(p, self$getParameterValue("df"))
  rand <- function(n) rchisq(n, self$getParameterValue("df"))

  if(df == 1)
    support <- PosReals$new(zero = F)
  else
    support <- PosReals$new(zero = T)

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = support,
                   symmetric  = FALSE,type = PosReals$new(zero = TRUE),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "ChiSq", ClassName = "ChiSquared",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
