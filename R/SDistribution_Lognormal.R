#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Lognormal Distribution Documentation
#-------------------------------------------------------------
#' @title Lognormal Distribution
#'
#' @description Mathematical and statistical functions for the Lognormal distribution parameterised
#' with mean, sd, var, prec on the log or natural scale. The logmean/logvar parameterisation is defined
#' by the pdf,
#' \deqn{exp(-(log(x)-\mu)^2/2\sigma^2)/(x\sigma\sqrt(2\pi))}
#' where \eqn{\mu \epsilon R} is the log-mean and \eqn{\sigma > 0} is the log-standard deviation.
#'
#' @details \code{cf} is omitted as no closed form analytic expression could be found. The \code{mgf} is
#' included but returns NaN as it is not defined.
#'
#' @name Lognormal
#'
#' @section Constructor: Lognormal$new(meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL,
#'                           mean = 1, var = NULL, sd = NULL, prec = NULL,
#'                           decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{meanlog} \tab numeric \tab mean of the distribution on the log scale. \cr
#' \code{varlog} \tab numeric \tab variance of the distribution on the log scale. \cr
#' \code{sdlog} \tab numeric \tab standard deviation of the distribution on the log scale. \cr
#' \code{preclog} \tab numeric \tab precision of the distribution on the log scale. \cr
#' \code{mean} \tab numeric \tab mean of the distribution on the natural scale. \cr
#' \code{var} \tab numeric \tab variance of the distribution on the natural scale. \cr
#' \code{sd} \tab numeric \tab standard deviation of the distribution on the natural scale. \cr
#' \code{precision} \tab numeric \tab precision of the distribution on the natural scale. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Lognormal distribution can either be parameterised with variance,
#' standard deviation or precision on the natural or log scale.
#' If none are provided then meanlog/varlog parameterisation is used with varlog = 1, meanlog = 0.
#' sdlog is defined by
#' \deqn{sdlog = varlog^2}
#' preclog is defined by
#' \deqn{preclog = varlog^-1}
#' The variance on the natural scale is given by
#' \deqn{var = (exp(var) - 1)) * exp(2 * meanlog + varlog)}
#' and \eqn{sd}, \eqn{prec} defined analogously.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Many parameterisations are possible
#' Lognormal$new(var = 2, mean = 1)
#' Lognormal$new(meanlog = 2, preclog = 5)
#' Lognormal$new(meanlog = 4, sd = 2) # Note parameters must be on same scale (log or natural)
#'
#' x <- Lognormal$new(verbose = TRUE) # meanlog = 0, sdlog = 1 default
#'
#' # Update parameters
#' x$setParameterValue(list(meanlog = 3)) # When any parameter is updated, all others are too!
#' x$parameters()
#'
#' # But you can only set parameters on the same scale, the below has no effect
#' x$setParameterValue(list(sd = 3))
#' x$setParameterValue(list(sdlog = 3)) # But this does
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
# Lognormal Distribution Definition
#-------------------------------------------------------------
Lognormal <- R6::R6Class("Lognormal", inherit = SDistribution, lock_objects = F)
Lognormal$set("public","name","Lognormal")
Lognormal$set("public","short_name","Lognormal")
Lognormal$set("public","traits",list(type = PosReals$new(),
                                     valueSupport = "continuous",
                                     variateForm = "univariate"))
Lognormal$set("public","description","Lognormal Probability Distribution.")
Lognormal$set("public","package","stats")

Lognormal$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Lognormal$set("public","var",function(){
  return(self$getParameterValue("var"))
})
Lognormal$set("public","skewness",function(){
  return(sqrt(exp(self$getParameterValue("varlog")) - 1) * (exp(self$getParameterValue("varlog")) + 2))
})
Lognormal$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return((exp(4 * self$getParameterValue("varlog")) + 2 * exp(3 * self$getParameterValue("varlog")) +
              3 * exp(2 *self$getParameterValue("varlog")) - 6))
  else
    return((exp(4 * self$getParameterValue("varlog")) + 2 * exp(3 * self$getParameterValue("varlog")) +
              3 * exp(2 *self$getParameterValue("varlog")) - 3))
})
Lognormal$set("public","entropy",function(base = 2){
  return(log(sqrt(2 * pi) * self$getParameterValue("sdlog") *
               exp(self$getParameterValue("meanlog") + 0.5), base))
})
Lognormal$set("public", "mgf", function(t){
  return(NaN)
})
Lognormal$set("public","mode",function(){
  return(exp(self$getParameterValue("meanlog")-self$getParameterValue("varlog")))
})

Lognormal$set("private",".getRefParams", function(paramlst){
  lst = list()

  if(!is.null(paramlst$meanlog)) meanlog <- paramlst$meanlog
  else meanlog <- self$getParameterValue("meanlog")
  if(!is.null(paramlst$varlog)) varlog <- paramlst$varlog
  else varlog <- self$getParameterValue("varlog")
  if(!is.null(paramlst$mean)) mean <- paramlst$mean
  else mean <- self$getParameterValue("mean")
  if(!is.null(paramlst$var)) var <- paramlst$var
  else var <- self$getParameterValue("var")

  if(self$parameters("meanlog")$settable){
    if(!is.null(paramlst[["meanlog"]])) lst = c(lst, list(meanlog = paramlst$meanlog))
    if(!is.null(paramlst[["varlog"]])) lst = c(lst, list(varlog = paramlst$varlog))
    if(!is.null(paramlst[["sdlog"]])) lst = c(lst, list(varlog = paramlst$sdlog^2))
    if(!is.null(paramlst[["preclog"]])) lst = c(lst, list(varlog = paramlst$preclog^-1))
  } else {
    if(!is.null(paramlst[["mean"]])) lst = c(lst, list(mean = paramlst$mean))
    if(!is.null(paramlst[["var"]])) lst = c(lst, list(var =  paramlst$var))
    if(!is.null(paramlst[["sd"]])) lst = c(lst, list(var = paramlst$sd^2))
    if(!is.null(paramlst[["prec"]])) lst = c(lst, list(var = paramlst$prec^-1))
  }

    return(lst)

})

Lognormal$set("public","initialize",function(meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL,
                                             mean = 1, var = NULL, sd = NULL, prec = NULL,
                                             decorators = NULL, verbose = FALSE){

  if(!is.null(var) | !is.null(sd) | !is.null(prec))
    meanlog = varlog = sdlog = preclog = NULL

  private$.parameters <- getParameterSet(self, meanlog, varlog, sdlog, preclog, mean, var, sd, prec, verbose)
  self$setParameterValue(list(meanlog = meanlog, varlog = varlog, sdlog = sdlog, preclog = preclog,
                              mean = mean, var = var, sd = sd, prec = prec))

  pdf <- function(x1) dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  cdf <- function(x1) plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  quantile <- function(p) qlnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  rand <- function(n) rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(), distrDomain = PosReals$new(),
                   symmetric = FALSE)
  invisible(self)
})

