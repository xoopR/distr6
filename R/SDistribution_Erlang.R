#-------------------------------------------------------------
# Erlang Distribution Documentation
#-------------------------------------------------------------
#' @name Erlang
#' @template SDist
#' @templateVar ClassName Erlang
#' @templateVar DistName Erlang
#' @templateVar uses as a special case of the Gamma distribution when the shape parameter is an integer
#' @templateVar params shape, \eqn{\alpha}, and rate, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)(x^{\alpha-1})(exp(-x\beta)) /(\alpha-1)!}
#' @templateVar paramsupport \eqn{\alpha = 1,2,3,\ldots} and \eqn{\beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor shape = 1, rate = 1, scale = NULL
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{rate} \tab numeric \tab inverse scale parameter. \cr
#' @templateVar arg3 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and either \code{rate} or \code{scale}, all as positive numerics. These are related via, \deqn{scale = 1/rate} If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Erlang$new(shape = 1, rate = 2)
#' Erlang$new(shape = 1, scale = 4)
#'
#' # Default is shape = 1, rate = 1
#' x = Erlang$new(verbose = TRUE)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(scale = 2)
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
# Erlang Distribution Definition
#-------------------------------------------------------------
Erlang <- R6Class("Erlang", inherit = SDistribution, lock_objects = F)
Erlang$set("public","name","Erlang")
Erlang$set("public","short_name","Erlang")
Erlang$set("public","description","Erlang Probability Distribution.")
Erlang$set("public","packages","stats")

Erlang$set("public","mean",function(){
  self$getParameterValue("shape")/self$getParameterValue("rate")
})
Erlang$set("public","variance",function(){
  self$getParameterValue("shape")/(self$getParameterValue("rate")^2)
})
Erlang$set("public","skewness",function() {
  2/sqrt(self$getParameterValue("shape"))
})
Erlang$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6/self$getParameterValue("shape"))
  else
    return((6/self$getParameterValue("shape"))+3)
})
Erlang$set("public","entropy",function(base = 2){
  (1-self$getParameterValue("shape"))*digamma(self$getParameterValue("shape")) +
    self$getParameterValue("shape") +
    log(gamma(self$getParameterValue("shape")/self$getParameterValue("rate")), base)
})
Erlang$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return((1-self$getParameterValue("scale")*t)^(-self$getParameterValue("shape")))
  else
    return(NaN)
})
Erlang$set("public", "pgf", function(z){
  return(NaN)
})
Erlang$set("public", "cf", function(t){
  (1-self$getParameterValue("scale")*1i*t)^(-self$getParameterValue("shape"))
})
Erlang$set("public","mode",function(which = NULL){
  (self$getParameterValue("shape")-1)/self$getParameterValue("rate")
})

Erlang$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))

  return(lst)
})
Erlang$set("private", ".pdf", function(x, log = FALSE){
  if(checkmate::testList(self$getParameterValue("shape"))){
    mapply(dgamma, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           MoreArgs = list(x = x, log = log))
  } else {
    dgamma(x, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"), log = log)
  }
})
Erlang$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE){
  if (checkmate::testList(self$getParameterValue("shape"))) {
    mapply(pgamma, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    pgamma(x, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           lower.tail = lower.tail, log.p = log.p)
  }
})
Erlang$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE){
  if (checkmate::testList(self$getParameterValue("shape"))) {
    mapply(qgamma, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    qgamma(p, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           lower.tail = lower.tail, log.p = log.p)
  }
})
Erlang$set("private", ".rand", function(n){
  if (checkmate::testList(self$getParameterValue("shape"))) {
    mapply(rgamma, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
           MoreArgs = list(n = n)
    )
  } else {
    rgamma(n, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"))
  }
})
Erlang$set("private", ".log", TRUE)

Erlang$set("public","initialize",function(shape = 1,rate = 1, scale = NULL, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.Erlang(self, shape, rate, scale, verbose)
  self$setParameterValue(shape = shape, rate = rate, scale = scale)

  super$initialize(decorators = decorators,
                   support = PosReals$new(zero = T),
                   type = PosReals$new(),
                   valueSupport = "continuous")
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Erlang", ClassName = "Erlang",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
