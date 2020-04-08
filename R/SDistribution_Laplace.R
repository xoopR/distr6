
#-------------------------------------------------------------
# Laplace Distribution Documentation
#-------------------------------------------------------------
#' @name Laplace
#' @template SDist
#' @templateVar ClassName Laplace
#' @templateVar DistName Laplace
#' @templateVar uses in signal processing and finance
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-|x-\mu|/\beta)/(2\beta)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor mean = 0, scale = 1, var = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{var} \tab numeric \tab alternate scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric and either \code{scale} or \code{var} as positive numerics. These are related via, \deqn{var = 2 * scale^2} If \code{var} is given then {scale} is ignored.
#'
#' @examples
#' Laplace$new(scale = 2)
#' Laplace$new(var = 4)
#'
#' x = Laplace$new(verbose = TRUE) # Default is mean = 0, scale = 1
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(var = 2)
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
# Laplace Distribution Definition
#-------------------------------------------------------------
Laplace <- R6Class("Laplace", inherit = SDistribution, lock_objects = F)
Laplace$set("public","name","Laplace")
Laplace$set("public","short_name","Lap")
Laplace$set("public","description","Laplace Probability Distribution.")
Laplace$set("public","packages","extraDistr")

Laplace$set("public","mean",function(){
  self$getParameterValue("mean")
})
Laplace$set("public","variance",function(){
  self$getParameterValue("var")
})
Laplace$set("public","skewness",function(){
  return(0)
})
Laplace$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(3)
  else
    return(6)
})
Laplace$set("public","entropy",function(base = 2){
  return(log(2 * exp(1) * self$getParameterValue("scale"), base))
})
Laplace$set("public", "mgf", function(t){
  if(abs(t) < 1/self$getParameterValue("scale"))
    return(exp(self$getParameterValue("mean") * t) / (1 - self$getParameterValue("scale")^2 * t^2))
  else
    return(NaN)
})
Laplace$set("public", "pgf", function(z){
  return(NaN)
})
Laplace$set("public", "cf", function(t){
  return(exp(self$getParameterValue("mean") * t * 1i) / (1 + self$getParameterValue("scale")^2 * t^2))
})
Laplace$set("public","mode",function(which = NULL){
  return(self$getParameterValue("mean"))
})

Laplace$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  if(!is.null(paramlst$var)) lst = c(lst, list(scale = sqrt(paramlst$var/2)))
  return(lst)
})
Laplace$set("private",".pdf", function(x){
  if(checkmate::testList(self$getParameterValue("mean"))){
    mapply(extraDistr::dlaplace,
           mu = self$getParameterValue("mean"),
           sigma = self$getParameterValue("scale"),
           MoreArgs = list(x = x, log = log))
  } else {
    extraDistr::dlaplace(x,
                         mu = self$getParameterValue("mean"),
                         sigma = self$getParameterValue("scale"),
                         log = log)
  }
})
Laplace$set("private",".cdf", function(x){
  if (checkmate::testList(self$getParameterValue("mean"))) {
    mapply(extraDistr::plaplace,
           mu = self$getParameterValue("mean"),
           sigma = self$getParameterValue("scale"),
           MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    extraDistr::plaplace(x,
                         mu = self$getParameterValue("mean"),
                         sigma = self$getParameterValue("scale"),
                         lower.tail = lower.tail, log.p = log.p)
  }
})
Laplace$set("private",".quantile", function(p){
  if (checkmate::testList(self$getParameterValue("mean"))) {
    mapply(extraDistr::qlaplace,
           mu = self$getParameterValue("mean"),
           sigma = self$getParameterValue("scale"),
           MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    extraDistr::qlaplace(p,
                         mu = self$getParameterValue("mean"),
                         sigma = self$getParameterValue("scale"),
                         lower.tail = lower.tail, log.p = log.p)
  }
})
Laplace$set("private",".rand", function(n){
  if (checkmate::testList(self$getParameterValue("mean"))) {
    mapply(extraDistr::rlaplace,
           mu = self$getParameterValue("mean"),
           sigma = self$getParameterValue("scale"),
           MoreArgs = list(n = n)
    )
  } else {
    extraDistr::rlaplace(n,
                         mu = self$getParameterValue("mean"),
                         sigma = self$getParameterValue("scale"))
  }
})
Laplace$set("private", ".log", TRUE)

Laplace$set("public","initialize",function(mean = 0, scale = 1, var = NULL,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, scale, var, verbose)
  self$setParameterValue(mean = mean, scale = scale, var = var)

  super$initialize(decorators = decorators,
                   support = Reals$new(),
                   symmetry = "sym",
                   type = Reals$new(),
                   valueSupport = "continuous")
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Lap", ClassName = "Laplace",
                                                     Type = "\u211D", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "extraDistr"))
