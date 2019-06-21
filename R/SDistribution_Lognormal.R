#' @include SetInterval_SpecialSet.R ParameterSet.R
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
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#'
#' @seealso See \code{\link{Distribution}} for inherited methods and variables. See \code{\link{DistributionDecorator}}
#' for Decorator details as well as \code{\link{CoreStatistics}} and \code{\link{ExoticStatistics}}.
#' @export
NULL
#-------------------------------------------------------------
# Lognormal Distribution Definition
#-------------------------------------------------------------
# Lognormal <- R6::R6Class("Lognormal", inherit = Distribution, lock_objects = F)
# Lognormal$set("public","name","Lognormal")
# Lognormal$set("public","short_name","lnorm")
# Lognormal$set("public","description","Lognormal Probability Distribution.")
# Lognormal$set("public","package","stats")
# 
# Lognormal$set("public","traits",list(type = Reals$new(),
#                                      valueSupport = "continuous",
#                                      variateForm = "multivariate"))
# 
# Lognormal$set("public","properties",list(support = PosReals$new(zero = T),
#                                          distrDomain = PosReals$new(zero = T),
#                                          symmetry  = "asymmetric"))
# 
# Lognormal$set("private",".pdf",function(x1, log = FALSE){
#   dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sd"), log)
# })
# 
# Lognormal$set("private",".cdf",function(x1, lower.tail = TRUE, log.p = FALSE){
#   plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"), lower.tail, log.p)
# })
# 
# Lognormal$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
#   qLnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"), lower.tail, log.p)
# })
# 
# Lognormal$set("private",".rand",function(n){
#   rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
# })
# 
# Lognormal$set("public","expectation",function(){
#   return(exp(self$getParameterValue("meanlog") + self$getParameterValue("sdlog")^2/2))
# })
# 
# Lognormal$set("public","variance",function(){
#   return(exp(self$getParameterValue("sdlog")^2 - 1) * exp(2 * self$getParameterValue("meanlog") + self$getParameterValue("sdlog")^2))
# })
# 
# Lognormal$set("public","varlog",function(){
#   self$getParameterValue("varlog")
# })
# 
# Lognormal$set("public","skewness",function() {
#   return(sqrt(exp(self$getParameterValue("sdlog")^2) - 1) * (exp(self$getParameterValue("sdlog")^2)) + 2)
# })
# 
# Lognormal$set("public","kurtosis",function(){
#   return(exp(4 * self$getParameterValue("sdlog")^2) + 2 * exp(3 * self$getParameterValue("sdlog")^2) +
#            3 * exp(2 *self$getParameterValue("sdlog")^2) - 6)
# })
# 
# Lognormal$set("public","entropy",function(base = 2){
#   return(log(self$getParameterValue("sdlog") * exp(self$getParameterValue("meanlog") + 0.5) * sqrt(2 * pi), base))
# })
# 
# Lognormal$set("public", "mgf", function(t){
#   return(NaN)
# })
# 
# Lognormal$set("public", "cf", function(t){
#   stop('No analytical solution, please use the CoreStatistics decorator')
# })
# 
# Lognormal$set("public","survival",function(x1, log.p = FALSE){
#   self$cdf(x1, lower.tail = FALSE, log.p)
# })
# 
# Lognormal$set("public","hazard",function(x1){
#   self$pdf(x1)/self$survival(x1)
# })
# 
# Lognormal$set("public","cumHazard",function(x1){
#   -self$cdf(x1, log.p = TRUE)
# })
# 
# Lognormal$set("public","mode",function() return((self$getParameterValue("meanlog") - self$getParameterValue("sdlog")^2)))
# 
# Lognormal$set("private",".parameters", NULL)
# 
# Lognormal$set("public","initialize",function(meanlog = 0, varlog = NULL, sdlog = NULL, preclog = NULL, decorators = NULL,...){
# 
#   varlog.bool = FALSE
#   sdlog.bool = FALSE
#   preclog.bool = FALSE
# 
#   if(is.null(varlog) & is.null(sdlog) & is.null(preclog)){
#     message("varlog, sdlog and preclog missing. varlog = 1 parameterisation used.")
#     varlog = 1
#   } else if(!is.null(varlog) & (!is.null(sdlog) | !is.null(preclog))){
#     message("Multiple parameterisations provided. varlog parameterisation used.")
#     varlog = varlog
#     sdlog = NULL
#     preclog = NULL
#   } else if(is.null(varlog) & !is.null(sdlog) & !is.null(preclog)){
#     message("Multiple parameterisations provided. sdlog parameterisation used.")
#     sdlog = sdlog
#     varlog = NULL
#     preclog = NULL
#   }
# 
#   if(!is.null(varlog)){
#     varlog.bool = TRUE
#     varlog.update = NA
#     sdlog.update = "self$getParameterValue('varlog')^0.5"
#     preclog.update = "self$getParameterValue('varlog')^-1"
#   } else if(!is.null(sdlog)){
#     sdlog.bool = TRUE
#     sdlog.update = NA
#     varlog.update = "self$getParameterValue('sdlog')^2"
#     preclog.update = "self$getParameterValue('sdlog')^-2"
#   } else{
#     preclog.bool = TRUE
#     preclog.update = NA
#     varlog.update = "self$getParameterValue('preclog')^-1"
#     sdlog.update = "self$getParameterValue('preclog')^-0.5"
#   }
# 
#   private$.parameters <- ParameterSet$new(id = list("meanlog","varlog","sdlog","preclog"),
#                                           value = list(0, 1, 1, 1),
#                                           lower = list(-Inf, 0, 0, 0),
#                                           upper = list(Inf, Inf, Inf, Inf),
#                                           class = list("numeric","numeric","numeric","numeric"),
#                                           settable = list(TRUE, varlog.bool, sdlog.bool, preclog.bool),
#                                           updateFunc = list(NA, varlog.update, sdlog.update, preclog.update),
#                                           description = list("Mean-log - Location Parameter on log scale",
#                                                              "Variance-log - Squared Scale Parameter on log scale",
#                                                              "Standard Deviation-log - Scale Parameter on log scale",
#                                                              "Precision-log - Inverse Squared Scale Parameter on log scale"))
# 
#   self$setParameterValue(list(meanlog = meanlog))
#   if(!is.null(varlog)) self$setParameterValue(list(varlog = varlog))
#   else if(!is.null(sdlog)) self$setParameterValue(list(sdlog = sdlog))
#   else if(!is.null(preclog)) self$setParameterValue(list(preclog = preclog))
# 
#   super$initialize(decorators = decorators,...)
#   invisible(self)
# })

Lognormal <- R6::R6Class("Lognormal", inherit = SDistribution, lock_objects = F)
Lognormal$set("public","name","Lognormal")
Lognormal$set("public","short_name","Lognormal")
Lognormal$set("public","traits",list(type = Reals$new(),
                                     valueSupport = "continuous",
                                     variateForm = "univariate"))
Lognormal$set("public","description","Lognormal Probability Distribution.")
Lognormal$set("public","package","stats")

Lognormal$set("public","meanlog",function(){
  return(self$getParameterValue("meanlog"))
})
Lognormal$set("public","varlog",function(){
  return(self$getParameterValue("varlog"))
})
Lognormal$set("public","skewness",function(){
  return(sqrt(exp(self$getParameterValue("sdlog")^2) - 1) * (exp(self$getParameterValue("sdlog")^2)) + 2)
})
Lognormal$set("public","kurtosis",function(excess = TRUE){
  return(exp(4 * self$getParameterValue("sdlog")^2) + 2 * exp(3 * self$getParameterValue("sdlog")^2) +
           3 * exp(2 *self$getParameterValue("sdlog")^2) - 6)
  # if(excess)
  #   return(0)
  # else
  #   return(3)
})
Lognormal$set("public","entropy",function(base = 2){
  return(0.5 * log(2 * pi * exp(1) * self$getParameterValue("varlog"), base))
})
Lognormal$set("public", "mgf", function(t){
  return(exp((self$getParameterValue("meanlog") * t) + (self$getParameterValue("varlog") * t^2 * 0.5)))
})
Lognormal$set("public", "cf", function(t){
  return(exp((1i * self$getParameterValue("meanlog") * t) - (self$getParameterValue("varlog") * t^2 * 0.5)))
})
Lognormal$set("public","mode",function(){
  return(self$getParameterValue("meanlog"))
})

Lognormal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$meanlog)) lst = c(lst, list(mean = paramlst$meanlog))
  if(!is.null(paramlst$varlog)) lst = c(lst, list(var = paramlst$varlog))
  if(!is.null(paramlst$preclog)) lst = c(lst, list(var = paramlst$preclog^-1))
  if(!is.null(paramlst$sdlog)) lst = c(lst, list(var = paramlst$sdlog^2))
  return(lst)
})

Lognormal$set("public","initialize",function(meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL,
                                             decorators = NULL, verbose = FALSE){
  
  private$.parameters <- getParameterSet(self, meanlog, varlog, sdlog, preclog, verbose)
  self$setParameterValue(list(meanlog = meanlog, varlog = varlog, sdlog = sdlog, preclog = preclog))
  
  pdf <- function(x1) dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  cdf <- function(x1) plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  quantile <- function(p) qlnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  rand <- function(n) rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})

