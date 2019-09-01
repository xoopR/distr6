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
#' @templateVar constructor df = 1, location = 0
#' @templateVar arg1 \code{df} \tab numeric \tab  degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab non-centrality parameter (ncp in rstats). \cr
#' @templateVar constructorDets \code{df} and \code{location} as non-negative numerics.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution.
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
  return(self$getParameterValue("df") + self$getParameterValue("location"))
})
ChiSquared$set("public", "variance", function(){
  return(2*(self$getParameterValue("df") + 2*self$getParameterValue("location")))
})
ChiSquared$set("public", "skewness", function(){
  df <- self$getParameterValue("df")
  ncp <- self$getParameterValue("location")
  if (df + ncp == 0)
    return(NaN)
  else
    return(((2^(3/2))*(df + 3*ncp))/((df + 2*ncp)^(3/2)))
})
ChiSquared$set("public", "kurtosis", function(excess = TRUE){
  df <- self$getParameterValue("df")
  ncp <- self$getParameterValue("location")
  if(excess)
    if (df + ncp == 0)
      return(NaN)
    else
      return((12*(df + 4*ncp))/((df + 2*ncp)^2))
  else
    if (df + ncp == 0)
      return(NaN)
    else
      return((12*(df + 4*ncp))/((df + 2*ncp)^2) + 3)
})
ChiSquared$set("public", "entropy", function(base = 2){
  if (self$getParameterValue("location") == 0)
    return(self$getParameterValue("df")/2 + log(2*gamma(self$getParameterValue("df")/2), base) +
      ((1 - self$getParameterValue("df")/2)*digamma(self$getParameterValue("df")/2)))
  else
    return(NaN)
})
ChiSquared$set("public", "mgf", function(t){
  if(t < 0.5){
    return(exp(self$getParameterValue("location")*t/(1 - 2*t))/((1 - 2*t)^(self$getParameterValue("df")/2)))
  } else{
    return(NaN)
  }
})
ChiSquared$set("public", "cf", function(t){
  return(exp(self$getParameterValue("location")*1i*t/(1 - 2i*t))/((1 - 2i*t)^(self$getParameterValue("df")/2)))
})
ChiSquared$set("public", "pgf", function(z){
  if (self$getParameterValue("location") == 0 & z > 0 & z < sqrt(exp(1)))
    return((1 - 2 * log(z))^(-self$getParameterValue("df")/2))
  else
    return(NaN)
})
ChiSquared$set("public", "mode", function(){
  if (self$getParameterValue("location") == 0)
    return(max(self$getParameterValue("df") - 2, 0))
  else
    return(NaN)
})

ChiSquared$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  if(self$getParameterValue("df") <= 1)
    private$.properties$support <- PosReals$new(zero = F)
  else
    private$.properties$support <- PosReals$new(zero = T)
  invisible(self)
})
ChiSquared$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})

ChiSquared$set("public","initialize",function(df = 1, location = 0, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df, location, verbose)
  self$setParameterValue(df = df, location = location)

  pdf <- function(x1) dchisq(x1, self$getParameterValue("df"), self$getParameterValue("location"))
  cdf <- function(x1) pchisq(x1, self$getParameterValue("df"), self$getParameterValue("location"))
  quantile <- function(p) qchisq(p, self$getParameterValue("df"), self$getParameterValue("location"))
  rand <- function(n) rchisq(n, self$getParameterValue("df"), self$getParameterValue("location"))

  if(df == 1)
    support <- PosReals$new(zero = F)
  else
    support <- PosReals$new(zero = T)

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = support,
                   symmetric  = FALSE, type = PosReals$new(zero = TRUE),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})
