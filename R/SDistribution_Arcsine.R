#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Arcsine Distribution Documentation
#-------------------------------------------------------------
#' @name Arcsine
#' @template SDist
#' @templateVar ClassName Arcsine
#' @templateVar DistName Arcsine
#' @templateVar uses in the study of random walks and as a special case of the Beta distribution
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(\pi\sqrt{(x-a)(b-x))}}
#' @templateVar paramsupport \eqn{-\infty < a \le b < \infty}
#' @templateVar distsupport \eqn{[a, b]}
#' @templateVar omittedVars \code{cf} and \code{mgf}
#' @templateVar additionalDetails When the Standard Arcsine is constructed (default) then \code{\link[stats]{rbeta}} is used for sampling, otherwise via inverse transform
#' @templateVar constructor lower = 0, upper = 1
#' @templateVar arg1 \code{lower} \tab integer \tab lower distribution limit. \cr
#' @templateVar arg2 \code{upper} \tab integer \tab upper distribution limit. \cr
#' @templateVar constructorDets \code{lower} and \code{upper} as numerics.
#' @templateVar additionalSeeAlso \code{\link{rbeta}} for the Beta distribution sampling function.
#'
#' @examples
#' x = Arcsine$new(lower = 2, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(upper = 4, lower = 1)
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
# Arcsine Distribution Definition
#-------------------------------------------------------------
Arcsine <- R6::R6Class("Arcsine", inherit = SDistribution, lock_objects = F)
Arcsine$set("public","name","Arcsine")
Arcsine$set("public","short_name","Arc")
Arcsine$set("public","description","Arcsine Probability Distribution.")
Arcsine$set("public","package","distr6")

Arcsine$set("public","mean",function(){
  return((self$getParameterValue("upper") + self$getParameterValue("lower"))/2)
})
Arcsine$set("public","variance",function(){
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
Arcsine$set("public", "pgf", function(z){
  return(NaN)
})

Arcsine$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$lower)) lst = c(lst, list(lower = paramlst$lower))
  if(!is.null(paramlst$upper)) lst = c(lst, list(upper = paramlst$upper))
  return(lst)
})

Arcsine$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= lst[["upper"]])
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"))
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"))

  super$setParameterValue(lst = lst, error = error)
  private$.properties$support <- Interval$new(self$getParameterValue("lower"),self$getParameterValue("upper"))
  invisible(self)
})

Arcsine$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, lower, upper, verbose)
  self$setParameterValue(lower = lower, upper = upper)

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
                   support = Interval$new(lower,upper),  symmetric = TRUE,
                   type = Reals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Arc", ClassName = "Arcsine",
                                               Type = "\u211D", ValueSupport = "continuous", VariateForm = "univariate",
                                               Package = "distr6"))

