#-------------------------------------------------------------
# Lognormal Distribution Documentation
#-------------------------------------------------------------
#' @title Lognormal Distribution
#'
#' @description Mathematical and statistical functions for the Lognormal distribution parameterised
#' with mean and standard deviation of the variable's natural logarithm.
#'
#' @name Lognormal
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{meanlog} \tab numeric \tab mean of the distribution on the log scale. \cr
#' \code{varlog} \tab numeric \tab variance of the distribution on the log scale. \cr
#' \code{sdlog} \tab numeric \tab standard deviation of the distribution on the log scale. \cr
#' \code{precisionlog} \tab numeric \tab parameter. \cr
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
#' the Lognormal distribution.
#'
#'
#' @section Public Variables:
#'  \tabular{lr}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab "Lognormal" \cr
#'   \code{short_name} \tab "lnorm" \cr
#'   \code{traits} \tab List of Lognormal distribution traits. \cr
#'   \code{properties} \tab List of Lognormal distribution properties. \cr
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
#-------------------------------------------------------------
# Normal Distribution Definition
#-------------------------------------------------------------
#' @include SetInterval_SpecialSet.R ParameterSet.R
#' @export
LogNormal <- R6::R6Class("Lognormal", inherit = Distribution, lock_objects = F)
LogNormal$set("public","name","Lognormal")
LogNormal$set("public","short_name","lnorm")
LogNormal$set("public","traits",list(type = Reals$new(),
                                     valueSupport = "continuous",
                                     variateForm = "multivariate"))

LogNormal$set("public","properties",list(support = PosReals$new(zero = T),
                                         distrDomain = PosReals$new(zero = T),
                                         symmetry  = "asymmetric"))

LogNormal$set("private",".pdf",function(x1, log = FALSE){
  dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sd"), log)
})

LogNormal$set("private",".cdf",function(x1, lower.tail = TRUE, log.p = FALSE){
  plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"), lower.tail, log.p)
})

LogNormal$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  qLnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"), lower.tail, log.p)
})

LogNormal$set("private",".rand",function(n){
  rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
})

LogNormal$set("public","expectation",function(){
  return(exp(self$getParameterValue("meanlog") + self$getParameterValue("sdlog")^2/2))
})

LogNormal$set("public","variance",function(){
  return(exp(self$getParameterValue("sdlog")^2 - 1) * exp(2 * self$getParameterValue("meanlog") + self$getParameterValue("sdlog")^2))
})

LogNormal$set("public","varlog",function(){
  self$getParameterValue("varlog")
})

LogNormal$set("public","skewness",function() {
  return(sqrt(exp(self$getParameterValue("sdlog")^2) - 1) * (exp(self$getParameterValue("sdlog")^2)) + 2)
})

LogNormal$set("public","kurtosis",function(){
  return(exp(4 * self$getParameterValue("sdlog")^2) + 2 * exp(3 * self$getParameterValue("sdlog")^2) +
           3 * exp(2 *self$getParameterValue("sdlog")^2) - 6)
})

LogNormal$set("public","entropy",function(base = 2){
  return(log(self$getParameterValue("sdlog") * exp(self$getParameterValue("meanlog") + 0.5) * sqrt(2 * pi), base))
})

LogNormal$set("public", "mgf", function(t){
  return(NaN)
})

LogNormal$set("public", "cf", function(t){
  stop('No analytical solution, please use the CoreStatistics decorator')
})

LogNormal$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1, lower.tail = FALSE, log.p)
})

LogNormal$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

LogNormal$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})

LogNormal$set("public","mode",function() return((self$getParameterValue("meanlog") - self$getParameterValue("sdlog")^2)))

LogNormal$set("private",".parameters", NULL)

LogNormal$set("public","initialize",function(meanlog = 0, varlog = NULL, sdlog = NULL, preclog = NULL, decorators = NULL,...){

  varlog.bool = FALSE
  sdlog.bool = FALSE
  preclog.bool = FALSE

  if(is.null(varlog) & is.null(sdlog) & is.null(preclog)){
    message("varlog, sdlog and preclog missing. varlog = 1 parameterisation used.")
    varlog = 1
  } else if(!is.null(varlog) & (!is.null(sdlog) | !is.null(preclog))){
    message("Multiple parameterisations provided. varlog parameterisation used.")
    varlog = varlog
    sdlog = NULL
    preclog = NULL
  } else if(is.null(varlog) & !is.null(sdlog) & !is.null(preclog)){
    message("Multiple parameterisations provided. sdlog parameterisation used.")
    sdlog = sdlog
    varlog = NULL
    preclog = NULL
  }

  if(!is.null(varlog)){
    varlog.bool = TRUE
    varlog.update = NA
    sdlog.update = "self$getParameterValue('varlog')^0.5"
    preclog.update = "self$getParameterValue('varlog')^-1"
  } else if(!is.null(sdlog)){
    sdlog.bool = TRUE
    sdlog.update = NA
    varlog.update = "self$getParameterValue('sdlog')^2"
    preclog.update = "self$getParameterValue('sdlog')^-2"
  } else{
    preclog.bool = TRUE
    preclog.update = NA
    varlog.update = "self$getParameterValue('preclog')^-1"
    sdlog.update = "self$getParameterValue('preclog')^-0.5"
  }

  private$.parameters <- ParameterSet$new(id = list("meanlog","varlog","sdlog","preclog"),
                                          value = list(0, 1, 1, 1),
                                          lower = list(-Inf, 0, 0, 0),
                                          upper = list(Inf, Inf, Inf, Inf),
                                          class = list("numeric","numeric","numeric","numeric"),
                                          settable = list(TRUE, varlog.bool, sdlog.bool, preclog.bool),
                                          updateFunc = list(NA, varlog.update, sdlog.update, preclog.update),
                                          description = list("Mean-log - Location Parameter on log scale",
                                                             "Variance-log - Squared Scale Parameter on log scale",
                                                             "Standard Deviation-log - Scale Parameter on log scale",
                                                             "Precision-log - Inverse Squared Scale Parameter on log scale"))

  self$setParameterValue(list(meanlog = meanlog))
  if(!is.null(varlog)) self$setParameterValue(list(varlog = varlog))
  else if(!is.null(sdlog)) self$setParameterValue(list(sdlog = sdlog))
  else if(!is.null(preclog)) self$setParameterValue(list(preclog = preclog))

  super$initialize(decorators = decorators,...)
  invisible(self)
})
