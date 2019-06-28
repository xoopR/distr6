#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Degenerate Distribution Documentation
#-------------------------------------------------------------
#' @title Degenerate Distribution
#'
#' @description Mathematical and statistical functions for the Degenerate distribution parameterised
#' by its mean and defined by the pdf,
#' \deqn{f(x) = 1, if x = \mu; 0 otherwise}
#' where \eqn{\mu \epsilon R} is the mean parameter.
#'
#' @details The Degenerate Distribution is also known as the Dirac Distribution.
#'
#' @name Degenerate
#'
#' @section Constructor: Degenerate$new(mean = 0, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab location parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Degenerate distribution is parameterised with a location parameter (mean),
#' default is 0.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Degenerate$new(mean = 4)
#'
#' # Update parameters
#' x$setParameterValue(list(mean = 2.56))
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
# Degenerate Distribution Definition
#-------------------------------------------------------------
Degenerate <- R6::R6Class("Degenerate", inherit = SDistribution, lock_objects = F)
Degenerate$set("public","name","Degenerate")
Degenerate$set("public","short_name","Degen")
Degenerate$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Degenerate$set("public","description","Degenerate Probability Distribution.")
Degenerate$set("public","package","distr6")

Degenerate$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Degenerate$set("public","var",function(){
  return(0)
})
Degenerate$set("public","skewness",function(){
  return(NaN)
})
Degenerate$set("public","kurtosis",function(excess = TRUE){
  return(NaN)
})
Degenerate$set("public","entropy",function(base = 2){
  return(0)
})
Degenerate$set("public", "mgf", function(t){
  return(exp(self$getParameterValue("mean") * t))
})
Degenerate$set("public", "cf", function(t){
  return(exp(self$getParameterValue("mean") * t * 1i))
})
Degenerate$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Degenerate$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  private$.properties$support <- Set$new(self$getParameterValue("mean"))
  invisible(self)
})

Degenerate$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  return(lst)
})


Degenerate$set("public","initialize",function(mean = 0, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, verbose)
  self$setParameterValue(list(mean = mean))

  pdf <- function(x1) if(x1 == self$getParameterValue("mean")) return(1) else return(0)
  cdf <- function(x1) if(x1 >= self$getParameterValue("mean")) return(1) else return(0)
  quantile <- function(p) if(p > 0) return(self$getParameterValue("mean")) else return(-Inf)
  rand <- function(n) return(rep(self$getParameterValue("mean"), n))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(mean), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})
