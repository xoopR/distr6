#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Noncentral Chi-Squared Distribution Documentation
#-------------------------------------------------------------
#' @name ChiSquaredNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName ChiSquaredNoncentral
#' @templateVar DistName Noncentral Chi-Squared
#' @templateVar uses to model the sum of independent squared Normal distributions and for confidence intervals
#' @templateVar params degrees of freedom, \eqn{\nu}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-\lambda/2) \sum_{r=0}^\infty ((\lambda/2)^r/r!) (x^{(\nu+2r)/2-1}exp(-x/2))/(2^{(\nu+2r)/2}\Gamma((\nu+2r)/2))}
#' @templateVar paramsupport \eqn{\nu \ge 0}, \eqn{\lambda \ge 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor df = 1, location = 0
#' @templateVar arg1 \code{df} \tab numeric \tab  degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab location (ncp in rstats). \cr
#' @templateVar omittedVars \code{entropy} and \code{mode}
#' @templateVar constructorDets \code{df} and \code{location} as non-negative numerics.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{ChiSquared}} for the central Chi-Squared distribution.
#'
#' @examples
#' x = ChiSquaredNoncentral$new(df = 2, location = 2)
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
# ChiSquaredNoncentral Distribution Definition
#-------------------------------------------------------------
ChiSquaredNoncentral <- R6::R6Class("ChiSquaredNoncentral", inherit = SDistribution, lock_objects = FALSE)
ChiSquaredNoncentral$set("public", "name", "ChiSquaredNoncentral")
ChiSquaredNoncentral$set("public", "short_name", "ChiSqNC")
ChiSquaredNoncentral$set("public", "description", "Noncentral ChiSquared Probability Distribution")
ChiSquaredNoncentral$set("public","package","stats")

ChiSquaredNoncentral$set("public", "mean", function(){
  return(self$getParameterValue("df") + self$getParameterValue("location"))
})
ChiSquaredNoncentral$set("public", "variance", function(){
  return(2*(self$getParameterValue("df") + 2*self$getParameterValue("location")))
})
ChiSquaredNoncentral$set("public", "skewness", function(){
  df <- self$getParameterValue("df")
  ncp <- self$getParameterValue("location")
  if (df + ncp == 0)
    return(NaN)
  else
    return(((2^(3/2))*(df + 3*ncp))/((df + 2*ncp)^(3/2)))
})
ChiSquaredNoncentral$set("public", "kurtosis", function(excess = TRUE){
  df <- self$getParameterValue("df")
  ncp <- self$getParameterValue("location")
  if (df + ncp == 0)
    return(NaN)
  else
    kur = (12*(df + 4*ncp))/((df + 2*ncp)^2)

  if(excess)
    return(kur)
  else
    return(kur + 3)
})
ChiSquaredNoncentral$set("public", "mgf", function(t){
  if(t < 0.5){
    return(exp(self$getParameterValue("location")*t/(1 - 2*t))/((1 - 2*t)^(self$getParameterValue("df")/2)))
  } else{
    return(NaN)
  }
})
ChiSquaredNoncentral$set("public", "cf", function(t){
  return(exp(self$getParameterValue("location")*1i*t/(1 - 2i*t))/((1 - 2i*t)^(self$getParameterValue("df")/2)))
})

ChiSquaredNoncentral$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  if(self$getParameterValue("df") <= 1)
    private$.properties$support <- PosReals$new(zero = F)
  else
    private$.properties$support <- PosReals$new(zero = T)
  invisible(self)
})
ChiSquaredNoncentral$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})

ChiSquaredNoncentral$set("public","initialize",function(df = 1, location = 0, decorators = NULL, verbose = FALSE){

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

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "ChiSqNC", ClassName = "ChiSquaredNoncentral",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))

