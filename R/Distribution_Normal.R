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
#' @section Constructor: Normal$new(mean = 0, var = NULL, sd = NULL, prec = NULL, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab mean, location parameter. \cr
#' \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' \code{precision} \tab numeric \tab precision, squared scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
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
Normal <- R6::R6Class("Normal", inherit = Distribution, lock_objects = F)
Normal$set("public","name","Normal")
Normal$set("public","short_name","Norm")
Normal$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))

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
Normal$set("public","initialize",function(mean = 0, var = NULL, sd = NULL, prec = NULL, decorators = NULL){

  var.bool = FALSE
  sd.bool = FALSE
  prec.bool = FALSE

  if(is.null(var) & is.null(sd) & is.null(prec)){
    message("var, sd and prec missing. var = 1 parameterisation used.")
    var = 1
  } else if(!is.null(var) & (!is.null(sd) | !is.null(prec))){
    message("Multiple parameterisations provided. var parameterisation used.")
    var = var
    sd = NULL
    prec = NULL
  } else if(is.null(var) & !is.null(sd) & !is.null(prec)){
    message("Multiple parameterisations provided. sd parameterisation used.")
    sd = sd
    var = NULL
    prec = NULL
  }

  if(!is.null(var)){
    var.bool = TRUE
    var.update = NA
    sd.update = "self$getParameterValue('var')^0.5"
    prec.update = "self$getParameterValue('var')^-1"
  } else if(!is.null(sd)){
    sd.bool = TRUE
    sd.update = NA
    var.update = "self$getParameterValue('sd')^2"
    prec.update = "self$getParameterValue('sd')^-2"
  } else{
    prec.bool = TRUE
    prec.update = NA
    var.update = "self$getParameterValue('prec')^-1"
    sd.update = "self$getParameterValue('prec')^-0.5"
  }

  private$.parameters <- ParameterSet$new(id = list("mean","var","sd","prec"),
                                          value = list(0, 1, 1, 1),
                                          lower = list(-Inf, 0, 0, 0),
                                          upper = list(Inf, Inf, Inf, Inf),
                                          class = list("numeric","numeric","numeric","numeric"),
                                          settable = list(TRUE, var.bool, sd.bool, prec.bool),
                                          updateFunc = list(NA, var.update, sd.update, prec.update),
                                          description = list("Mean - Location Parameter",
                                                             "Variance - Squared Scale Parameter",
                                                             "Standard Deviation - Scale Parameter",
                                                             "Precision - Inverse Squared Scale Parameter"))

  self$setParameterValue(list(mean = mean))
  if(!is.null(var)) self$setParameterValue(list(var = var))
  else if(!is.null(sd)) self$setParameterValue(list(sd = sd))
  else if(!is.null(prec)) self$setParameterValue(list(prec = prec))

  pdf <- function(x1) dnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  cdf <- function(x1) pnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"))
  quantile <- function(p) qnorm(p, self$getParameterValue("mean"), self$getParameterValue("sd"))
  rand <- function(n) rnorm(n, self$getParameterValue("mean"), self$getParameterValue("sd"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})
