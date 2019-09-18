#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Noncentral Student's t Distribution Documentation
#-------------------------------------------------------------
#' @name StudentTNoncentral
#' @template SDist
#' @templateVar ClassName StudentTNoncentral
#' @templateVar DistName Noncentral Student's T
#' @templateVar uses to estimate the mean of populations with unknown variance from a small sample size, as well as in t-testing for difference of means and regression analysis
#' @templateVar distsupport the Reals
#' @templateVar constructor df = 1, location = 0
#' @templateVar arg1 \code{df} \tab numeric \tab degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab non-centrality parameter (ncp in rstats). \cr
#' @templateVar omittedVars \code{skewness}, \code{kurtosis}, \code{mode}, \code{entropy}, \code{pgf}, \code{mgf} and \code{cf}
#' @templateVar constructorDets \code{df} and \code{location} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{StudentT}} for the central Student's T distribution.
#'
#' @examples
#' x = StudentTNoncentral$new(df = 2, location = 3)
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
# Noncentral Student's t Distribution Definition
#-------------------------------------------------------------
StudentTNoncentral <- R6::R6Class("StudentTNoncentral", inherit = SDistribution, lock_objects = F)
StudentTNoncentral$set("public","name","StudentTNoncentral")
StudentTNoncentral$set("public","short_name","TNC")
StudentTNoncentral$set("public","description","Student's t Probability Distribution.")
StudentTNoncentral$set("public","package","stats")

StudentTNoncentral$set("public", "mean", function(){
  df <- self$getParameterValue("df")
  if (df > 1)
    return(self$getParameterValue("location")*sqrt(df/2)*gamma((df - 1)/2)/gamma(df/2))
  else
    return(NaN)
})
StudentTNoncentral$set("public","variance",function(){
  df <- self$getParameterValue("df")
  mu <- self$getParameterValue("location")
  if(df > 2)
    return(df*(1 + mu^2)/(df-2) - (mu^2*df/2)*(gamma((df - 1)/2)/gamma(df/2))^2)
  else
    return(NaN)
})

StudentTNoncentral$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})

StudentTNoncentral$set("public","initialize",function(df = 1, location = 0, decorators = NULL, verbose = FALSE){
  
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
