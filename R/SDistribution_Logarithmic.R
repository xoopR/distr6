
#-------------------------------------------------------------
# Logarithmic Distribution Documentation
#-------------------------------------------------------------
#' @name Logarithmic
#' @template SDist
#' @templateVar ClassName Logarithmic
#' @templateVar DistName Logarithmic
#' @templateVar uses to model consumer purchase habits in economics and is derived from the Maclaurin series expansion of \eqn{-ln(1-p)}
#' @templateVar params a parameter, \eqn{\theta},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = -\theta^x/xlog(1-\theta)}
#' @templateVar paramsupport \eqn{0 < \theta < 1}
#' @templateVar distsupport \eqn{{1,2,3,\ldots}}
#' @templateVar omittedVars \code{entropy}
#' @templateVar additionalDetails The distribution is implemented by interfacing the \code{extraDistr} package.
#' @templateVar constructor theta = 0.5
#' @templateVar arg1 \code{theta} \tab numeric \tab theta parameter. \cr
#' @templateVar constructorDets \code{theta} as a number between 0 and 1.
#' @templateVar additionalSeeAlso \code{\link[extraDistr]{LogSeries}} for the d/p/q/r implementation.
#'
#' @examples
#' x = Logarithmic$new(theta = 0.2)
#'
#' # Update parameters
#' x$setParameterValue(theta = 0.3)
#' x$parameters()
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
# Logarithmic Distribution Definition
#-------------------------------------------------------------
Logarithmic <- R6Class("Logarithmic", inherit = SDistribution, lock_objects = F)
Logarithmic$set("public","name","Logarithmic")
Logarithmic$set("public","short_name","Log")
Logarithmic$set("public","description","Logarithmic Probability Distribution.")
Logarithmic$set("public","packages","extraDistr")

Logarithmic$set("public","mean",function(){
  theta = self$getParameterValue("theta")
  return(-theta/(log(1-theta)*(1-theta)))
})
Logarithmic$set("public","variance",function(){
  theta = self$getParameterValue("theta")
  return((-theta^2 - theta*log(1-theta)) / ((1-theta)^2 * (log(1-theta))^2))
})
Logarithmic$set("public","mode",function(which = "all"){
  return(1)
})
Logarithmic$set("public","skewness",function(){
  theta = self$getParameterValue("theta")

  s1 = (theta*(3*theta + theta*log(1-theta) + log(1-theta))) / ((theta-1)^3 * log(1-theta)^2)
  s2 = 2 * (-theta/(log(1-theta)*(1-theta)))^3

  return((s1+s2)/(self$stdev()^3))
})
Logarithmic$set("public","kurtosis",function(excess = TRUE){
  theta = self$getParameterValue("theta")

  s1 = (3*theta^4)/((1-theta)^4*log(1-theta)^4)
  s2 = (6*theta^3)/((theta-1)^4*log(1-theta)^3)
  s3 = (4*theta^3)/((theta-1)^4*log(1-theta)^2)
  s4 = (theta^3)/((theta-1)^4*log(1-theta))
  s5 = (4*theta^2)/((theta-1)^4*log(1-theta)^2)
  s6 = (4*theta^2)/((theta-1)^4*log(1-theta))
  s7 = (theta)/((theta-1)^4*log(1-theta))

  sum = - s1 - s2 - s3 - s4 - s5 - s6 - s7

  kurtosis = sum/(self$stdev()^4)

  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})
Logarithmic$set("public", "mgf", function(t){
  if(t < -log(self$getParameterValue("theta")))
    return(log(1-self$getParameterValue("theta")*exp(t))/log(1-self$getParameterValue("theta")))
  else
    return(NaN)
})
Logarithmic$set("public", "cf", function(t){
  return(log(1-self$getParameterValue("theta")*exp(t*1i))/log(1-self$getParameterValue("theta")))
})
Logarithmic$set("public", "pgf", function(z){
  if(abs(z) < 1/self$getParameterValue("theta"))
    return(log(1-self$getParameterValue("theta")*z)/log(1-self$getParameterValue("theta")))
  else
    return(NaN)
})

Logarithmic$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$theta)) lst = c(lst,list(theta = paramlst$theta))
  return(lst)
})
Logarithmic$set("private", ".pdf", function(x, log = FALSE){
  if(checkmate::testList(self$getParameterValue("theta"))){
    mapply(extraDistr::dlgser, theta = self$getParameterValue("theta"),
           MoreArgs = list(x = x, log = log))
  } else {
    extraDistr::dlgser(x, theta = self$getParameterValue("theta"), log = log)
  }
})
Logarithmic$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE){
  if (checkmate::testList(self$getParameterValue("theta"))) {
    mapply(extraDistr::plgser, theta = self$getParameterValue("theta"),
           MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    extraDistr::plgser(x, theta = self$getParameterValue("theta"),
           lower.tail = lower.tail, log.p = log.p)
  }
})
Logarithmic$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE){
  if (checkmate::testList(self$getParameterValue("theta"))) {
    mapply(extraDistr::qlgser, theta = self$getParameterValue("theta"),
           MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    extraDistr::qlgser(p, theta = self$getParameterValue("theta"),
           lower.tail = lower.tail, log.p = log.p)
  }
})
Logarithmic$set("private", ".rand", function(n){
  if (checkmate::testList(self$getParameterValue("theta"))) {
    mapply(extraDistr::rlgser, theta = self$getParameterValue("theta"),
           MoreArgs = list(n = n)
    )
  } else {
    extraDistr::rlgser(n, theta = self$getParameterValue("theta"))
  }
})
Logarithmic$set("private", ".log", TRUE)
Logarithmic$set("private", ".traits", list(valueSupport = "discrete", variateForm = "univariate"))

Logarithmic$set("public", "initialize", function(theta = 0.5, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet.Logarithmic(self, theta, verbose)
  self$setParameterValue(theta=theta)

  super$initialize(decorators = decorators,
                   support = PosNaturals$new(),
                   type = Naturals$new())
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Log", ClassName = "Logarithmic",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "extraDistr"))

