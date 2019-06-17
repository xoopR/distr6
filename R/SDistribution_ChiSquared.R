#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Chi-Squared Distribution Documentation
#-------------------------------------------------------------
#' @title Chi-Squared Distribution
#'
#' @description Mathematical and statistical functions for the Chi-Squared distribution parameterised
#' with degrees of freedom. The Chi-Squared distribution is defined by the pdf,
#' \deqn{f(x) = (x^(\nu/2-1) exp(-x/2))/(2^(\nu/2) * Gamma(\nu/2))}
#' where \eqn{\nu > 0} is the degrees of freedom.
#'
#' @name ChiSquared
#'
#' @section Constructor: ChiSquared$new(df = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{df} \tab numeric \tab degrees of freedom. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Chi-Squared distribution is parameterised with
#' degrees of freedom, df. Default parameterisation is with df = 1.
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Normal Statistical Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @export
NULL
ChiSquared <- R6::R6Class("ChiSquared", inherit = SDistribution, lock_objects = FALSE)
ChiSquared$set("public", "name", "ChiSquared")
ChiSquared$set("public", "short_name", "ChiSq")
ChiSquared$set("public", "traits", list(type = PosReals$new(zero = TRUE),
                                        valueSupport = "continuous",
                                        variateForm = "univariate"))
ChiSquared$set("public", "description", "ChiSquared Probability Distribution")

ChiSquared$set("public", "mean", function(){
  self$getParameterValue("df")
})
ChiSquared$set("public", "var", function(){
  self$getParameterValue("df")*2
})
ChiSquared$set("public", "skewness", function(){
  sqrt(8/self$getParameterValue("df"))
})
ChiSquared$set("public", "kurtosis", function(excess = TRUE){
  exkurtosis = 12/self$getParameterValue("df")
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
ChiSquared$set("public", "entropy", function(base = 2){
  self$getParameterValue("df")/2 + log(2*gamma(self$getParameterValue("df")/2), base) +
    (1 - self$getParameterValue("df")/2)*digamma(self$getParameterValue("df")/2)
})
ChiSquared$set("public", "mgf", function(t){
  if(t < 1/2){
    (1 - 2*t)^(-self$getParameterValue("df")/2)
  } else{
    return(NaN)
  }
})
ChiSquared$set("public", "cf", function(t){
  (1 - 2i*t)^(-self$getParameterValue("df")/2)
})
ChiSquared$set("public", "mode", function(){
  max(self$getParameterValue("df") - 2, 0)
})


pdf <- function(x1) dchisq(x1, self$getParameterValue("df"))
cdf <- function(x1) pchisq(x1, self$getParameterValue("df"))
quantile <- function(p) qchisq(p, self$getParameterValue("df"))
rand <- function(n) rchisq(n, self$getParameterValue("df"))
