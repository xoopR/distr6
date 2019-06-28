#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Arcsine Distribution Documentation
#-------------------------------------------------------------
#' @title Arcsine Distribution
#'
#' @description Mathematical and statistical functions for the Arcsine distribution parameterised
#' with lower and upper limits. The Arcsine distribution is defined by the pdf,
#' \deqn{f(x) = 1/(\pi\sqrt((x-a)(b-x)))}
#' where \eqn{-\infty < a \le b < \infty} are the lower and upper limits respectively.
#'
#' @details The cf and mgf are omitted as no closed form analytic expression could be found.
#'
#' @name Arcsine
#'
#' @section Constructor: Arcsine$new(lower = 0, upper = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{lower} \tab numeric \tab lower distribution limit. \cr
#' \code{upper} \tab numeric \tab upper distribution limit. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Arcsine distribution is parameterised with default support of
#' \eqn{[0,1]}. Both the \code{lower} and \code{upper} arguments must be finite.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Arcsine$new(lower = 2, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(upper = 4, lower = 1))
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
# Arcsine Distribution Definition
#-------------------------------------------------------------
Arcsine <- R6::R6Class("Arcsine", inherit = SDistribution, lock_objects = F)
Arcsine$set("public","name","Arcsine")
Arcsine$set("public","short_name","Arc")
Arcsine$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Arcsine$set("public","description","Arcsine Probability Distribution.")
Arcsine$set("public","package","distr6")

Arcsine$set("public","mean",function(){
  return((self$getParameterValue("upper") + self$getParameterValue("lower"))/2)
})
Arcsine$set("public","var",function(){
  return(((self$getParameterValue("upper") - self$getParameterValue("lower"))^2)/8)
})
Arcsine$set("public","skewness",function(){
  return(0)
})
Arcsine$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-3/2)
  else
    return(1.5)
})
Arcsine$set("public","entropy",function(base = 2){
  return(log(pi/4, base))
})
Arcsine$set("public","mode",function(which = "all"){
  if(which == "all")
    return(c(self$getParameterValue("lower"),self$getParameterValue("upper")))
  else
    return(c(self$getParameterValue("lower"),self$getParameterValue("upper"))[which])
})

Arcsine$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$lower)) lst = c(lst, list(lower = paramlst$lower))
  if(!is.null(paramlst$upper)) lst = c(lst, list(upper = paramlst$upper))
  return(lst)
})

Arcsine$set("public","setParameterValue",function(lst, error = "warn"){
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= lst[["upper"]])
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"))
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"))

  super$setParameterValue(lst, error)
  private$.properties$support <- Interval$new(self$getParameterValue("lower"),self$getParameterValue("upper"))
})

Arcsine$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, lower, upper, verbose)
  self$setParameterValue(list(lower = lower, upper = upper))

  pdf <- function(x1){
    if(self$getParameterValue("lower")==0 & self$getParameterValue("upper") == 1)
      return(dbeta(x1, 0.5, 0.5))
    else
      return((pi * sqrt((x1 - self$getParameterValue("lower")) * (self$getParameterValue("upper") - x1)))^-1)
  }

  cdf <- function(x1){
    if(self$getParameterValue("lower")==0 & self$getParameterValue("upper") == 1)
      return(pbeta(x1, 0.5, 0.5))
    else
      return((2/pi) * (asin(sqrt(
      (x1 - self$getParameterValue("lower")) /
      (self$getParameterValue("upper") - self$getParameterValue("lower"))))))
  }

  quantile <- function(p){
    if(self$getParameterValue("lower")==0 & self$getParameterValue("upper") == 1)
      return(qbeta(p, 0.5, 0.5))
    else
      return(((self$getParameterValue("upper") - self$getParameterValue("lower")) *
                sin(p * pi * 0.5)^2) +
               self$getParameterValue("lower"))
  }

  rand <- function(n){
    if(self$getParameterValue("lower")==0 & self$getParameterValue("upper") == 1)
      return(rbeta(n, 0.5, 0.5))
    else
      return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Interval$new(lower,upper), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
})
