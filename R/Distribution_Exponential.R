#' @title Exponential Distribution
#' @description Mathematical and statistical functions for the exponential distribution parameterised
#' with rate or scale.
#' @name Exponential
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{rate} \tab numeric \tab arrival rate. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{...} \tab ANY \tab additional arguments for Distribution constructor. See details. \cr
#' }
#'
#' @section Constructor Details: The exponential distribution can either be parameterised with a rate or
#' scale parameter. If neither are provided then rate parameterisation is used with rate = 1. If both are
#' provided then rate parameterisation is used with given rate. Scale is defined by
#' \deqn{scale = 1/rate}
#' The CoreStatistics and ExoticStatistics decorators can be added to the distribution for further
#' numeric functionality, but these are approximate calculations only. Additional arguments can be passed
#' to the Distribution constructor, including R62S3 to determine if S3 methods should be added for
#' the exponential distribution.
#'
#'
#' @section Public Variables:
#'  \tabular{lr}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab "Exponential" \cr
#'   \code{short_name} \tab "Exp" \cr
#'   \code{traits} \tab List of exponential distribution traits. \cr
#'   \code{properties} \tab List of exponential distribution properties. \cr
#'   }
#'
#' @section Public Methods:
#'  \tabular{lrr}{
#'   \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#'   \code{pdf(x, log = FALSE)} \tab character \tab Evaluates density at x. \cr
#'   \code{cdf(q, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evaluates distribution function at q. \cr
#'   \code{quantile(p, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evalutes inverse distribution at p.  \cr
#'   \code{rand(n)} \tab numeric \tab Randomly generates n samples from the distribution.  \cr
#'   \code{expectation()} \tab numeric \tab Expectation.  \cr
#'   \code{var()} \tab numeric \tab Variance.  \cr
#'   \code{skewness()} \tab numeric \tab Skewness. \cr
#'   \code{kurtosis(excess = TRUE)} \tab numeric \tab Kurtosis. Kurtosis - 3 if excess = TRUE. \cr
#'   \code{entropy(base = 2)} \tab numeric \tab Entropy. Shannon if base = 2. \cr
#'   \code{mode()} \tab numeric \tab Mode. \cr
#'   \code{mgf(t)} \tab numeric \tab Evaluates moment generating function at t. \cr
#'   \code{cf(t)} \tab numeric \tab Evaluates characteristic function at t. \cr
#'   \code{survival(q, log.p = FALSE)} \tab numeric \tab Evaluates survival function at q. \cr
#'   \code{hazard(x)} \tab numeric \tab Evaluates hazard function at t. \cr
#'   \code{cumHazard(x)} \tab numeric \tab Evaluates cumulative hazard function at t. \cr
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
Exponential <- R6::R6Class("Exponential", inherit = Distribution, lock_objects = F)
Exponential$set("public","name","Exponential")
Exponential$set("public","short_name","Exp")
Exponential$set("public","traits",list(type = PosReals$new(zero = T),
                                    valueSupport = "continuous",
                                    variateForm = "univariate"))

Exponential$set("public","properties",list(support = PosReals$new(zero = T),
                                           distrDomain = PosReals$new(zero = T),
                                           symmetry  = "asymmetric"))

Exponential$set("public","pdf",function(x, log = FALSE)
  dexp(x, self$getParameterValue("rate"), log))

Exponential$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE)
  pexp(q, self$getParameterValue("rate"), lower.tail, log.p))

Exponential$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE)
  qexp(p, self$getParameterValue("rate"), lower.tail, log.p))

Exponential$set("public","rand",function(n)
  rexp(n, self$getParameterValue("rate")))

Exponential$set("public","expectation",function()
  self$getParameterValue("scale"))

Exponential$set("public","var",function()
  self$getParameterValue("scale")^2)

Exponential$set("public","skewness",function() return(2))

Exponential$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6)
  else
    return(9)
})

Exponential$set("public","entropy",function(base = 2){
  1 - log(self$getParameterValue("rate"), base)
})

Exponential$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return(self$getParameterValue("rate") / (self$getParameterValue("rate") - t))
  else
    return(0)
})

Exponential$set("public", "cf", function(t){
  return(self$getParameterValue("rate") / (self$getParameterValue("rate") -  ((0+1i) * t)))
})

Exponential$set("public","survival",function(q, log.p = FALSE)
  self$cdf(q, lower.tail = FALSE, log.p))

Exponential$set("public","hazard",function(x)
  self$pdf(x)/self$survival(x))

Exponential$set("public","cumHazard",function(x)
  -self$cdf(x, log.p = TRUE))

Exponential$set("public","mode",function() return(0))

Exponential$set("private",".parameters", NULL)


Exponential$set("public","initialize",function(rate = NULL, scale = NULL, decorators = NULL,...){

  rate.bool = FALSE
  scale.bool = FALSE

  if(is.null(rate) & is.null(scale)){
    message("rate and scale missing. rate = 1 parameterisation used.")
    rate = 1
  } else if(!is.null(rate) & !is.null(scale)){
    message("Both rate and scale provided. rate parameterisation used.")
    rate = rate
    scale = NULL
  }

  if(!is.null(rate)){
    rate.bool = TRUE
    rate.update = NA
    scale.update = "self$getParameterValue('rate')^-1"
  } else{
    scale.bool = TRUE
    scale.update = NA
    rate.update = "self$getParameterValue('scale')^-1"
  }

  private$.parameters <- ParameterSet$new(id = list("rate","scale"), value = list(1, 1),
                   lower = list(0, 0), upper = list(Inf, Inf),
                   class = list("numeric","numeric"),
                   settable = list(rate.bool, scale.bool),
                   fittable = list(rate.bool, scale.bool),
                   updateFunc = list(rate.update, scale.update),
                   description = list("Arrival Rate", "Scale"))

  if(!is.null(rate)) self$setParameterValue(list(rate = rate))
  if(!is.null(scale)) self$setParameterValue(list(scale = scale))

  super$initialize(decorators = decorators,...)
  invisible(self)
})
