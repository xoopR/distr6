#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Normal Distribution Documentation
#-------------------------------------------------------------
#' @title Normal Distribution
#'
#' @description Mathematical and statistical functions for the Normal distribution parameterised
#' with mean and variance, sd or precision.
#'
#' @name Normal
#'
#' @section Constructor: Normal$new(mean = 0, var = NULL, sd = NULL, prec = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{ } \tab numeric \tab mean, location parameter. \cr
#' \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' \code{precision} \tab numeric \tab precision, squared scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Normal distribution can either be parameterised with variance,
#' standard deviation or precision. If none are provided then var parameterisation is used with var = 1.
#' If multiple are provided then parameterisatin takes the hierarchy: var, sd, prec.
#' sd is defined by
#' \deqn{sd = var^2}
#' prec is defined by
#' \deqn{prec = var^-1}
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#'
#' @export
NULL
#-------------------------------------------------------------
# Normal Distribution Definition
#-------------------------------------------------------------
Normal <- R6::R6Class("Normal", inherit = SDistribution, lock_objects = F)
Normal$set("public","name","Normal")
Normal$set("public","short_name","Norm")
Normal$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Normal$set("public","description","Normal Probability Distribution.")

Normal$set("public","mean",function(){
  self$getParameterValue("mean")
})
Normal$set("public","var",function(){
  self$getParameterValue("var")
})
Normal$set("public","skewness",function(){
  return(0)
})
Normal$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(0)
  else
    return(3)
})
Normal$set("public","entropy",function(base = 2){
  return(0.5 * log(2 * pi * exp(1) * self$getParameterValue("var"), base))
})
Normal$set("public", "mgf", function(t){
  return(exp((self$getParameterValue("mean") * t) + (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public", "cf", function(t){
  return(exp((1i * self$getParameterValue("mean") * t) - (self$getParameterValue("var") * t^2 * 0.5)))
})
Normal$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})

Normal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$var)) lst = c(lst, list(var = paramlst$var))
  if(!is.null(paramlst$prec)) lst = c(lst, list(var = paramlst$prec^-1))
  if(!is.null(paramlst$sd)) lst = c(lst, list(var = paramlst$sd^2))
  return(lst)
})

Normal$set("public","initialize",function(mean = 0, var = 1, sd = NULL, prec = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, var, sd, prec, verbose)
  self$setParameterValue(list(mean = mean, var = var, sd = sd, prec = prec))

  pdf <- function(x1) dnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  cdf <- function(x1) pnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  quantile <- function(p) qnorm(p, self$getParameterValue("mean"), self$getParameterValue("sd"))
  rand <- function(n) rnorm(n, self$getParameterValue("mean"), self$getParameterValue("sd"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})
