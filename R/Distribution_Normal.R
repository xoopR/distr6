#' @title Normal Distribution
#' @description Mathematical and statistical functions for the Normal distribution parameterised
#' with rate or scale.
#' @name Normal
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab mean, location parameter. \cr
#' \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' \code{precision} \tab numeric \tab precision, squared scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{...} \tab ANY \tab additional arguments for Distribution constructor. See details. \cr
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
#' The CoreStatistics and ExoticStatistics decorators can be added to the distribution for further
#' numeric functionality, but these are approximate calculations only. Additional arguments can be passed
#' to the Distribution constructor, including R62S3 to determine if S3 methods should be added for
#' the Normal distribution.
#'
#'
#' @section Public Variables:
#'  \tabular{lr}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab "Normal" \cr
#'   \code{short_name} \tab "norm" \cr
#'   \code{traits} \tab List of Normal distribution traits. \cr
#'   \code{properties} \tab List of Normal distribution properties. \cr
#'   }
#'
#' @section Public Methods:
#'  \tabular{lrr}{
#'   \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#'   \code{pdf(x1, log = FALSE)} \tab character \tab Evaluates density at x1. \cr
#'   \code{cdf(x1, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evaluates distribution function at x1. \cr
#'   \code{quantile(p, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evalutes inverse distribution at p.  \cr
#'   \code{rand(n)} \tab numeric \tab Randomly generates n samples from the distribution.  \cr
#'   \code{normectation()} \tab numeric \tab normectation.  \cr
#'   \code{var()} \tab numeric \tab Variance.  \cr
#'   \code{skewness()} \tab numeric \tab Skewness. \cr
#'   \code{kurtosis(excess = TRUE)} \tab numeric \tab Kurtosis. Kurtosis - 3 if excess = TRUE. \cr
#'   \code{entropy(base = 2)} \tab numeric \tab Entropy. Shannon if base = 2. \cr
#'   \code{mode()} \tab numeric \tab Mode. \cr
#'   \code{mgf(t)} \tab numeric \tab Evaluates moment generating function at t. \cr
#'   \code{cf(t)} \tab numeric \tab Evaluates characteristic function at t. \cr
#'   \code{survival(x1, log.p = FALSE)} \tab numeric \tab Evaluates survival function at x1. \cr
#'   \code{hazard(x1)} \tab numeric \tab Evaluates hazard function at x1. \cr
#'   \code{cumHazard(x1)} \tab numeric \tab Evaluates cumulative hazard function at x1. \cr
#'   }
#'
#' @section Public Methods Details:
#' If \code{log.p} is TRUE then the natural logarithm of probabilities is returned. If \code{lower.tail}
#' is TRUE then distribution functions are evaluated at the lower tail of the distribution, otherwise
#' the upper tail (1 - p).
#'
#'
#' @seealso See \code{\link{Distribution}} for inherited methods and variables. See \code{\link{DistributionDecorator}}
#' for Decorator details as well as \code{\link{CoreStatistics}} and \code{\link{ExoticStatistics}}.
NULL

#' @include SetInterval_SpecialSet.R ParameterSet.R
#' @export
Normal <- R6::R6Class("Normal", inherit = Distribution, lock_objects = F)
Normal$set("public","name","Normal")
Normal$set("public","short_name","Norm")
Normal$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))

Normal$set("public","properties",list(support = Reals$new(zero = T),
                                      distrDomain = Reals$new(zero = T),
                                      symmetry  = "symmetric"))

Normal$set("private",".pdf",function(x1, log = FALSE){
  dnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"), log))
}

Normal$set("private",".cdf",function(x1, lower.tail = TRUE, log.p = FALSE){
  pnorm(x1, self$getParameterValue("mean"), self$getParameterValue("sd"), lower.tail, log.p))
}

Normal$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  qnorm(p, self$getParameterValue("mean"), self$getParameterValue("sd"), lower.tail, log.p))
}

Normal$set("private",".rand",function(n){
  rnorm(n, self$getParameterValue("mean"), self$getParameterValue("sd")))
}

Normal$set("public","expectation",function(){
  self$getParameterValue("mean"))
}

Normal$set("public","var",function(){
  self$getParameterValue("var"))
}

Normal$set("public","skewness",function() return(0))

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

Normal$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1, lower.tail = FALSE, log.p))
}

Normal$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1))
}

Normal$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE))
}

Normal$set("public","mode",function() return(self$getParameterValue("mean")))

Normal$set("private",".parameters", NULL)

Normal$set("public","initialize",function(mean = 0, var = NULL, sd = NULL, prec = NULL, decorators = NULL,...){

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
                                          fittable = list(TRUE, var.bool, sd.bool, prec.bool),
                                          updateFunc = list(NA, var.update, sd.update, prec.update),
                                          description = list("Mean - Location Parameter",
                                                             "Variance - Squared Scale Parameter",
                                                             "Standard Deviation - Scale Parameter",
                                                             "Precision - Inverse Squared Scale Parameter"))

  self$setParameterValue(list(mean = mean))
  if(!is.null(var)) self$setParameterValue(list(var = var))
  else if(!is.null(sd)) self$setParameterValue(list(sd = sd))
  else if(!is.null(prec)) self$setParameterValue(list(prec = prec))

  super$initialize(decorators = decorators,...)
  invisible(self)
})
