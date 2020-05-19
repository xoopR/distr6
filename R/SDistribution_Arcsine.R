
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
Arcsine <- R6Class("Arcsine", inherit = SDistribution, lock_objects = F)
Arcsine$set("public","name","Arcsine")
Arcsine$set("public","short_name","Arc")
Arcsine$set("public","description","Arcsine Probability Distribution.")

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
Arcsine$set("private",".pdf",function(x, log = FALSE){
  lower = self$getParameterValue("lower")
  upper = self$getParameterValue("upper")

  pdf = matrix(nrow = nrow(x), ncol = length(lower))
  for(i in seq_along(lower)){
    for(j in 1:nrow(x)) {
      if(!log){
        pdf[j,i] = (pi * sqrt((x[j] - lower[i]) * (upper[i] - x[j])))^-1
      } else {
        pdf[j,i] = -log(pi) - log(x[j]-lower[i])/2 - log(upper[i]-x[j])/2
      }
    }
  }

  return(pdf)
})
Arcsine$set("private",".cdf",function(x, lower.tail = TRUE, log.p = FALSE){
  lower = self$getParameterValue("lower")
  upper = self$getParameterValue("upper")

  cdf = matrix(nrow = nrow(x), ncol = length(lower))
  for(i in seq_along(lower)){
    for(j in 1:nrow(x)) {
      cdf[j,i] = (2/pi) * (asin(sqrt((x - lower) / (upper - lower))))
    }
  }

  if(!lower.tail) cdf = 1 - cdf
  if(log.p) cdf = log(cdf)

  return(cdf)
})
Arcsine$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  lower = self$getParameterValue("lower")
  upper = self$getParameterValue("upper")

  if(log.p) p = exp(p)
  if(!lower.tail) p = 1 - p

  quantile = matrix(nrow = nrow(x), ncol = length(lower))
  for(i in seq_along(lower)){
    for(j in 1:nrow(x)) {
      quantile[j,i] = ((upper - lower) * (sin(p * pi * 0.5)^2)) + lower
    }
  }

  return(quantile)
})
Arcsine$set("private",".rand",function(n){
  self$quantile(runif(n))
})
Arcsine$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

Arcsine$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, lower, upper, verbose)
  self$setParameterValue(lower = lower, upper = upper)

  super$initialize(decorators = decorators,
                   support = Interval$new(lower,upper),
                   symmetry = "sym",
                   type = Reals$new())
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Arc", ClassName = "Arcsine",
                                               Type = "\u211D", ValueSupport = "continuous", VariateForm = "univariate",
                                               Package = "-"))

