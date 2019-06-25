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
#' \code{mean} \tab numeric \tab mean of the distribution on the natural scale. \cr
#' \code{var} \tab numeric \tab variance of the distribution on the natural scale. \cr
#' \code{sd} \tab numeric \tab standard deviation of the distribution on the natural scale. \cr
#' \code{precision} \tab numeric \tab parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{...} \tab ANY \tab additional arguments for Distribution constructor. See details. \cr
#' }
#'
#' @section Constructor Details: The Lognormal distribution can either be parameterised with variance,
#' standard deviation or precision on the natural scale or log scale.
#' If none are provided then var parameterisation is used with varlog = 1.
#' If multiple are provided then parameterisation takes the hierarchy: varlog, sdlog, preclog, 
#' var, sd, prec.
#' sdlog is defined by
#' \deqn{sd = varlog^2}
#' prec is defined by
#' \deqn{preclog = varlog^-1}
#' The variance on the natural scale is given by
#' \deqn{var = (exp(var) - 1)) * exp(2 * meanlog + varlog)}
#' \deqn{sd = var^2}
#' prec is defined by
#' \deqn{prec = var^-1} 
#' The mean on the natural scale is given by
#' \deqn{mean = exp(meanlog + varlog/2)}
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
  if(!is.null(paramlst$meanlog)) lst = c(lst, list(meanlog = paramlst$meanlog))
  if(!is.null(paramlst$varlog)) lst = c(lst, list(varlog = paramlst$varlog))
  if(!is.null(paramlst$sdlog)) lst = c(lst, list(varlog = paramlst$sdlog^2))
  if(!is.null(paramlst$preclog)) lst = c(lst, list(varlog = paramlst$preclog^-1))
  if(!is.null(paramlst$mean)) lst = c(lst, list(meanlog = log(paramlst$mean/sqrt(1 + paramlst$var/paramlst$mean^2))))
  if(!is.null(paramlst$var)) lst = c(lst, list(varlog = log(1 + paramlst$var/paramlst$mean^2)))
  if(!is.null(paramlst$sd)) lst = c(lst, list(var = paramlst$sd^2))
  if(!is.null(paramlst$prec)) lst = c(lst, list(var = paramlst$prec^-1))
  return(lst)
})

Lognormal$set("public","initialize",function(meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL,
                                             mean = NULL, var = NULL, sd = NULL, prec = NULL,
                                             decorators = NULL, verbose = FALSE){
  
  private$.parameters <- getParameterSet(self, meanlog, varlog, sdlog, preclog, mean, var, sd, prec, verbose)
  self$setParameterValue(list(meanlog = meanlog, varlog = varlog, sdlog = sdlog, preclog = preclog,
                              mean = mean, var = var, sd = sd, prec = prec))
  
  pdf <- function(x1) dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  cdf <- function(x1) plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  quantile <- function(p) qlnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  rand <- function(n) rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric = TRUE)
  invisible(self)
})

