
#-------------------------------------------------------------
# Degenerate Distribution Documentation
#-------------------------------------------------------------
#' @name Degenerate
#' @template SDist
#' @templateVar ClassName Degenerate
#' @templateVar DistName Degenerate
#' @templateVar uses to model deterministic events or as a representation of the delta, or Heaviside, function
#' @templateVar params mean, \eqn{\mu}
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1, \ if \ x = \mu}{f(x) = 1, if x = \mu}\deqn{f(x) = 0, \ if \ x \neq \mu}{f(x) = 0, if x != \mu}
#' @templateVar paramsupport \eqn{\mu \epsilon R}
#' @templateVar distsupport \eqn{{\mu}}
#' @templateVar aka Dirac
#' @aliases Dirac Delta
#' @templateVar constructor mean = 0
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric.
#'
#' @examples
#' x = Degenerate$new(mean = 4)
#'
#' # Update parameters
#' x$setParameterValue(mean = 2.56)
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
# Degenerate Distribution Definition
#-------------------------------------------------------------
Degenerate <- R6Class("Degenerate", inherit = SDistribution, lock_objects = F)
Degenerate$set("public","name","Degenerate")
Degenerate$set("public","short_name","Degen")
Degenerate$set("public","description","Degenerate Probability Distribution.")

Degenerate$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Degenerate$set("public","variance",function(){
  return(0)
})
Degenerate$set("public","skewness",function(){
  return(NaN)
})
Degenerate$set("public","kurtosis",function(excess = TRUE){
  return(NaN)
})
Degenerate$set("public","entropy",function(base = 2){
  return(0)
})
Degenerate$set("public", "mgf", function(t){
  return(exp(self$getParameterValue("mean") * t))
})
Degenerate$set("public", "cf", function(t){
  return(exp(self$getParameterValue("mean") * t * 1i))
})
Degenerate$set("public","mode",function(which = NULL){
  return(self$getParameterValue("mean"))
})

Degenerate$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Set$new(self$getParameterValue("mean"))
  invisible(self)
})

Degenerate$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  return(lst)
})
Degenerate$set("private", ".pdf", function(x, log = FALSE){
  if(x == self$getParameterValue("mean")){
    if(log) return(0) else return(1)
  } else {
    if(log) return(-Inf) else return(0)
  }
})
Degenerate$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE){
  cdf = if(x >= self$getParameterValue("mean")) 1 else 0
  if(lower.tail) cdf = 1 - cdf
  if(log.p) cdf = log(cdf)

  return(cdf)
})
Degenerate$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE){
  if(log.p) p = exp(p)
  if(lower.tail) p = 1 - p
  if(p > 0) return(self$getParameterValue("mean")) else return(-Inf)
})
Degenerate$set("private", ".rand", function(n){
  rep(self$getParameterValue("mean"), n)
})
Degenerate$set("private", ".log", TRUE)

Degenerate$set("public","initialize",function(mean = 0, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, verbose)
  self$setParameterValue(mean = mean)

  super$initialize(decorators = decorators,
                   support = Set$new(mean, class = "integer"),
                   symmetry = "sym",
                   type = Reals$new())
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Degen", ClassName = "Degenerate",
                                                     Type = "\u211D", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "-"))
