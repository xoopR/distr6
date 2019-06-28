#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Chi-Squared Distribution Documentation
#-------------------------------------------------------------
#' @title Chi-Squared Distribution
#'
#' @description Mathematical and statistical functions for the Chi-Squared distribution parameterised
#' with degrees of freedom. The Chi-Squared distribution is defined by the pdf,
#' \deqn{f(x) = (x^(\nu/2-1) exp(-x/2))/(2^(\nu/2) * \Gamma(\nu/2))}
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
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = ChiSquared$new(df = 2)
#'
#' # Update parameters
#' x$setParameterValue(list(df = 3))
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$var()
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
ChiSquared$set("public", "traits", list(type = PosReals$new(zero = TRUE),
                                        valueSupport = "continuous",
                                        variateForm = "univariate"))
ChiSquared$set("public", "description", "ChiSquared Probability Distribution")
ChiSquared$set("public","package","stats")

ChiSquared$set("public", "mean", function(){
  return(self$getParameterValue("df"))
})
ChiSquared$set("public", "var", function(){
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

ChiSquared$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  if(self$getParameterValue("df") == 1)
    private$.properties$support <- PosReals$new(zero = F)
  else
    private$.properties$support <- PosReals$new(zero = T)
})
ChiSquared$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  return(lst)
})

ChiSquared$set("public","initialize",function(df = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df, verbose)
  self$setParameterValue(list(df = df))

  pdf <- function(x1) dchisq(x1, self$getParameterValue("df"))
  cdf <- function(x1) pchisq(x1, self$getParameterValue("df"))
  quantile <- function(p) qchisq(p, self$getParameterValue("df"))
  rand <- function(n) rchisq(n, self$getParameterValue("df"))

  if(df == 1)
    support <- PosReals$new(zero = F)
  else
    support <- PosReals$new(zero = T)

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = support, distrDomain = Reals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})
