#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Uniform Distribution Documentation
#-------------------------------------------------------------
#' @title Uniform Distribution
#'
#' @description Mathematical and statistical functions for the Uniform distribution parameterised
#' with lower and upper limits. The Uniform distribution is defined by the pdf,
#' \deqn{f(x) = 1/(b-a)}
#' where \eqn{-\infty < a < b < \infty}, are the lower and upper limits respectively.
#'
#' @name Uniform
#'
#' @section Constructor: Uniform$new(lower = 0, upper = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{lower} \tab integer \tab lower distribution limit. \cr
#' \code{upper} \tab integer \tab upper distribution limit. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Uniform distribution is parameterised with lower = 0 and
#' upper = 1 limits respectively.
#'
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#'
#' @examples
#' x <- Uniform$new(lower = -10, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(lower = 2, upper = 7))
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
# Uniform Distribution Definition
#-------------------------------------------------------------
Uniform <- R6::R6Class("Uniform", inherit = SDistribution, lock_objects = F)
Uniform$set("public","name","Uniform")
Uniform$set("public","short_name","Unif")
Uniform$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Uniform$set("public","description","Uniform Probability Distribution.")
Uniform$set("public","package","stats")


Uniform$set("public","mean",function(){
 return((self$getParameterValue("lower")+self$getParameterValue("upper"))/2)
})
Uniform$set("public","var",function(){
  return(((self$getParameterValue("upper")-self$getParameterValue("lower"))^2)/12)
})
Uniform$set("public","skewness",function(){
  return(0)
})
Uniform$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-6/5)
  else
    return(1.8)
})
Uniform$set("public","entropy",function(base = 2){
  return(log(self$getParameterValue("upper")-self$getParameterValue("lower"), base))
})
Uniform$set("public", "mgf", function(t){
  if(t==0)
    return(1)
  else
    return((exp(self$getParameterValue("upper") * t) - exp(self$getParameterValue("lower") * t)) /
             (t*(self$getParameterValue("upper")-self$getParameterValue("lower"))))
})
Uniform$set("public", "cf", function(t){
  if(t==0)
    return(1)
  else
    return((exp(self$getParameterValue("upper") * t * 1i) - exp(self$getParameterValue("lower") * t * 1i)) /
             (t*1i*(self$getParameterValue("upper")-self$getParameterValue("lower"))))
})
Uniform$set("public","mode",function(){
  return(NaN)
})

Uniform$set("public","setParameterValue",function(lst, error = "warn"){
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] < lst[["upper"]], .var.name = "lower must be < upper")
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] < self$getParameterValue("upper"), .var.name = "lower must be < upper")
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] > self$getParameterValue("lower"), .var.name = "upper must be > lower")

  super$setParameterValue(lst, error)
  private$.properties$support <- Interval$new(self$getParameterValue("lower"), self$getParameterValue("upper"))
  invisible(self)
})
Uniform$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$lower)) lst = c(lst, list(lower = paramlst$lower))
  if(!is.null(paramlst$upper)) lst = c(lst, list(upper = paramlst$upper))
  return(lst)
})

Uniform$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, lower, upper, verbose)
  self$setParameterValue(list(lower = lower, upper = upper))

  pdf <- function(x1) dunif(x1, self$getParameterValue("lower"), self$getParameterValue("upper"))
  cdf <- function(x1) punif(x1, self$getParameterValue("lower"), self$getParameterValue("upper"))
  quantile <- function(p) qunif(p, self$getParameterValue("lower"), self$getParameterValue("upper"))
  rand <- function(n) runif(n, self$getParameterValue("lower"), self$getParameterValue("upper"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(lower, upper),
                   distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
})
