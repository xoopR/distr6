#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Logistic Distribution Documentation
#-------------------------------------------------------------
#' @title Logistic Distribution
#'
#' @description Mathematical and statistical functions for the Logistic distribution parameterised
#' with location and scale.
#'
#' @name Logistic
#'
#' @section Constructor: Logistic$new(location = 0, scale = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{ } \tab numeric \tab location, location parameter. \cr
#' \code{scale} \tab numeric \tab scale, scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Logistic distribution can either be parameterised with scaleiance,
#' standard deviation or precision. If none are provided then scale parameterisation is used with scale = 1.

#'
#' @inheritSection Distribution Public scaleiables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{location()} \tab \code{\link{location.Distribution}} \cr
#'   \code{scale()} \tab \code{\link{scale}} \cr
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
# Logistic Distribution Definition
#-------------------------------------------------------------
Logistic <- R6::R6Class("Logistic", inherit = SDistribution, lock_objects = F)
Logistic$set("public","name","Logistic")
Logistic$set("public","short_name","Norm")
Logistic$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "uniscaleiate"))
Logistic$set("public","description","Logistic Probability Distribution.")

Logistic$set("public","mean",function(){
  return(self$getParameterValue("location"))
})
Logistic$set("public","var",function(){
  s = getParameterValue("scale")
  return((s^2)*(pi^2)/3)
})

Logistic$set("public","skewness",function(){
  return(0)
})
Logistic$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6/5)
  else
    return(6/5 + 3)
})
Logistic$set("public","entropy",function(base = 2){
  return(2 + log(self$getParameterValue("scale"), base))
})
Logistic$set("public", "mgf", function(t){
  if (-1/s < t & t < 1/s) {
    mu <- self$getParameterValue("location")
    s <- self$getParametersValue("scale")
    return(exp(mu * t) * beta(1-s*t, 1+s*t))
  }
  else {
    return(NaN)
  }
  # note that the above will return finite real values
  # only when -1/s < t < 1/s
})
Logistic$set("public", "cf", function(t){
  mu <- self$getParameterValue("location")
  s <- self$getParametersValue("scale")
  return(exp((1i*mu*t)*(pi*s*t)/(sinh(pi*s*t))))
})
Logistic$set("public","mode",function(){
  return(self$getParameterValue("location"))
})

Logistic$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Logistic$set("public","initialize",function(location = 0, scale = 1,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, location, scale, verbose)
  self$setParameterValue(list(location = location, scale = scale))

  pdf <- function(x1) dlogis(x1, self$getParameterValue("location"), self$getParameterValue("scale"), log)
  cdf <- function(x1) plogis(x1, self$getParameterValue("location"), self$getParameterValue("scale"), lower.tail, log.p)
  quantile <- function(p) qlogis(p, self$getParameterValue("location"), self$getParameterValue("scale"), lower.tail, log.p)
  rand <- function(n) rlogis(n, self$getParameterValue("location"), self$getParameterValue("scale"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})
