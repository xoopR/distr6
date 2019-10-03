#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Lognormal Distribution Documentation
#-------------------------------------------------------------
#' @name Lognormal
#' @template SDist
#' @templateVar ClassName Lognormal
#' @templateVar DistName Log-Normal
#' @templateVar uses to model many natural phenomena as a result of growth driven by small percentage changes
#' @templateVar params logmean, \eqn{\mu}, and logvar, \eqn{\sigma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{exp(-(log(x)-\mu)^2/2\sigma^2)/(x\sigma\sqrt(2\pi))}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\sigma > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{cf}
#' @templateVar aka Log-Gaussian
#' @aliases Loggaussian
#' @templateVar constructor meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL, mean = 1, var = NULL, sd = NULL, prec = NULL
#' @templateVar arg1 \code{meanlog} \tab numeric \tab mean of the distribution on the log scale. \cr
#' @templateVar arg2 \code{varlog} \tab numeric \tab variance of the distribution on the log scale. \cr
#' @templateVar arg3 \code{sdlog} \tab numeric \tab standard deviation of the distribution on the log scale. \cr
#' @templateVar arg4 \code{preclog} \tab numeric \tab precision of the distribution on the log scale. \cr
#' @templateVar arg5 \code{mean} \tab numeric \tab mean of the distribution on the natural scale. \cr
#' @templateVar arg6 \code{var} \tab numeric \tab variance of the distribution on the natural scale. \cr
#' @templateVar arg7 \code{sd} \tab numeric \tab standard deviation of the distribution on the natural scale. \cr
#' @templateVar arg8 \code{prec} \tab numeric \tab precision of the distribution on the natural scale. \cr
#' @templateVar constructorDets either \code{meanlog} and \code{varlog}, \code{sdlog} or \code{preclog}, or \code{mean} and \code{var}, \code{sd} or \code{prec}. These are related via \deqn{var = (exp(var) - 1)) * exp(2 * meanlog + varlog)} \deqn{sdlog = varlog^2} \deqn{sd = var^2} Analogously for \code{prec} and \code{preclog}. If \code{prec} is given then all other parameters other than \code{mean} are ignored. If \code{sd} is given then all other parameters (except \code{prec}) are ignored. If \code{var} is given then all log parameters are ignored. If \code{preclog} is given then \code{varlog} and \code{sdlog} are ignored. Finally if \code{sdlog} is given then \code{varlog} is ignored.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution.
#'
#' @examples
#' # Many parameterisations are possible
#' Lognormal$new(var = 2, mean = 1)
#' Lognormal$new(meanlog = 2, preclog = 5)
#' # Note parameters must be on same scale (log or natural)
#' Lognormal$new(meanlog = 4, sd = 2)
#'
#' x <- Lognormal$new(verbose = TRUE) # meanlog = 0, sdlog = 1 default
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(meanlog = 3)
#' x$parameters()
#'
#' # But you can only set parameters on the same scale, the below has no effect
#' x$setParameterValue(sd = 3)
#' # But this does
#' x$setParameterValue(sdlog = 3)
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Lognormal Distribution Definition
#-------------------------------------------------------------
Lognormal <- R6::R6Class("Lognormal", inherit = SDistribution, lock_objects = F)
Lognormal$set("public","name","Log-Normal")
Lognormal$set("public","short_name","Lnorm")
Lognormal$set("public","description","Log-Normal Probability Distribution.")
Lognormal$set("public","package","stats")

Lognormal$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Lognormal$set("public","variance",function(){
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
Lognormal$set("public", "pgf", function(z){
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
  self$setParameterValue(meanlog = meanlog, varlog = varlog, sdlog = sdlog, preclog = preclog,
                              mean = mean, var = var, sd = sd, prec = prec)

  pdf <- function(x1) dlnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  cdf <- function(x1) plnorm(x1, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  quantile <- function(p) qlnorm(p, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))
  rand <- function(n) rlnorm(n, self$getParameterValue("meanlog"), self$getParameterValue("sdlog"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(),
                   symmetric = FALSE,type = PosReals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Lnorm", ClassName = "Lognormal",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
