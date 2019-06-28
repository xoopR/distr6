#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Beta Distribution Documentation
#-------------------------------------------------------------
#' @title Beta Distribution
#'
#' @description Mathematical and statistical functions for the Beta distribution parameterised
#' with two shape parameters and defined by the pdf,
#' \deqn{f(x) = (x^(\alpha-1)(1-x)^{\beta-1}) / B(\alpha, \beta)}
#' where \eqn{\alpha, \beta > 0} are the two shape parameters and \eqn{B} is the Beta function.
#'
#' @details \code{mgf} and \code{cf} are omitted as no analytic expression could be found. Decorate
#' with CoreStatistics for numeric results.
#'
#' @name Beta
#'
#' @section Constructor: Beta$new(shape1 = 1, shape2 = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape1} \tab numeric \tab positive shape parameter. \cr
#' \code{shape2} \tab numeric \tab positive shape parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Beta distribution is parameterised with two shape parameters,
#' both take the default value 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Beta$new(shape1 = 2, shape2 = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(shape1 = 1))
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
# Beta Distribution Definition
#-------------------------------------------------------------
Beta <- R6::R6Class("Beta", inherit = SDistribution, lock_objects = F)
Beta$set("public","name","Beta")
Beta$set("public","short_name","Beta")
Beta$set("public", "traits",list(type = PosReals$new(zero = T),
                                 valueSupport ="continuous",
                                 variateForm = "univariate"))
Beta$set("public","description","Beta Probability Distribution.")
Beta$set("public","package","stats")


Beta$set("public","mean",function(){
  return(self$getParameterValue("shape1") / (self$getParameterValue("shape1") + self$getParameterValue("shape2")))
})
Beta$set("public","var",function(){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")

  return(shape1*shape2*((shape1+shape2)^-2)*(shape1+shape2+1)^-1)
})
Beta$set("public","mode",function(which = "all"){

  if(self$getParameterValue("shape1")<=1 & self$getParameterValue("shape2")>1)
    return(0)
  else if(self$getParameterValue("shape1")>1 & self$getParameterValue("shape2")<=1)
    return(1)
  else if(self$getParameterValue("shape1")<1 & self$getParameterValue("shape2")<1){
    if(which == "all")
      return(c(0,1))
    else
      return(c(0,1)[which])
  } else if(self$getParameterValue("shape1")>1 & self$getParameterValue("shape2")>1)
    return((self$getParameterValue("shape1")-1)/(self$getParameterValue("shape1")+self$getParameterValue("shape2")-2))

})
Beta$set("public","skewness",function(){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")

  return(2*(shape2-shape1)*((shape1+shape2+1)^0.5)*((shape1+shape2+2)^-1)*((shape1*shape2)^-0.5))
})
Beta$set("public","kurtosis",function(excess = TRUE){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")

  ex_kurtosis = 6*{((shape1-shape2)^2)*(shape1+shape2+1)-(shape1*shape2*(shape1+shape2+2))}/
    (shape1*shape2*(shape1+shape2+2)*(shape1+shape2+3))
  if (excess)
    return(ex_kurtosis)
  else
    return(ex_kurtosis+3)
})
Beta$set("public", "entropy", function(base = 2){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  return(log(beta(shape1,shape2), base) - ((shape1-1)*digamma(shape1)) -
           ((shape2-1) * digamma(shape2)) + ((shape1+shape2-2)*digamma(shape1+shape2)))
})

Beta$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape1)) lst = c(lst,list(shape1 = paramlst$shape1))
  if(!is.null(paramlst$shape2)) lst = c(lst,list(shape2 = paramlst$shape2))
  return(lst)
})

Beta$set("public", "initialize", function(shape1 = 1, shape2 = 1, decorators = NULL,
                                          verbose = FALSE){

  private$.parameters <- getParameterSet.Beta(self, shape1, shape2, verbose)
  self$setParameterValue(list(shape1=shape1,shape2=shape2))

  pdf <- function(x1) dbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  cdf <- function(x1) pbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  quantile <- function(p) qbeta(p, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  rand <- function(n) rbeta(n, self$getParameterValue("shape1"), self$getParameterValue("shape2"))


  if (shape1 == shape2)
    symmetric <- TRUE
  else
    symmetric <- FALSE

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(0,1), distrDomain = PosReals$new(zero = TRUE),
                   symmetric = symmetric)

  invisible(self)
})
