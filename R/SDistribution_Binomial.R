
#-------------------------------------------------------------
# Binomial Distribution Documentation
#-------------------------------------------------------------
#' @name Binomial
#' @template SDist
#' @templateVar ClassName Binomial
#' @templateVar DistName Binomial
#' @templateVar uses to model the number of successes out of a number of independent trials
#' @templateVar params number of trials, n, and probability of success, p,
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(n, x)p^x(1-p)^{n-x}}
#' @templateVar paramsupport \eqn{n = 0,1,2,\ldots} and \eqn{p \ \epsilon \ [0,1]}{p \epsilon [0,1]}, where \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{{0, 1,...,n}}
#' @templateVar constructor size = 10, prob = 0.5, qprob = NULL
#' @templateVar arg1 \code{size} \tab numeric \tab number of trials. \cr
#' @templateVar arg2 \code{prob} \tab numeric \tab probability of success. \cr
#' @templateVar arg3 \code{qprob} \tab numeric \tab probability of failure. \cr
#' @templateVar constructorDets \code{size} as a whole number, and either \code{prob} or \code{qprob} as a number between 0 and 1. These are related via, \deqn{qprob = 1 - prob} If \code{qprob} is given then \code{prob} is ignored.
#'
#' @examples
#' # Can be parameterised with probability of success or failure
#' Binomial$new(prob = 0.2)
#' Binomial$new(qprob = 0.3)
#'
#' x = Binomial$new() # Default is with prob = 0.5 and size = 10
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(size = 4, qprob = 0.1)
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
# Binomial Distribution Definition
#-------------------------------------------------------------
Binomial <- R6Class("Binomial", inherit = SDistribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","description","Binomial Probability Distribution.")
Binomial$set("public","packages","stats")

Binomial$set("public","mean",function(){
  self$getParameterValue("size") * self$getParameterValue("prob")
})
Binomial$set("public","mode",function(which = NULL){
  return(floor((self$getParameterValue("size") + 1) * self$getParameterValue("prob")))
})
Binomial$set("public","variance",function(){
  self$getParameterValue("size") * self$getParameterValue("prob") * self$getParameterValue("qprob")
})
Binomial$set("public","skewness",function(){
  (1 - (2*self$getParameterValue("prob"))) / self$stdev()
})
Binomial$set("public","kurtosis",function(excess = TRUE){
  exkurtosis = (1 - (6*self$getParameterValue("prob") * self$getParameterValue("qprob"))) / self$variance()
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
Binomial$set("public","entropy",function(base = 2){
  0.5 * log(2 * pi * exp(1) * self$variance(), base)
})
Binomial$set("public", "mgf", function(t){
  (self$getParameterValue("qprob") + (self$getParameterValue("prob") * exp(t)))^self$getParameterValue("size")
})
Binomial$set("public", "cf", function(t){
  (self$getParameterValue("qprob") + (self$getParameterValue("prob") * exp((0+1i) * t)))^self$getParameterValue("size")
})
Binomial$set("public","pgf",function(z){
  (self$getParameterValue("qprob") + (self$getParameterValue("prob") * z))^self$getParameterValue("size")
})

Binomial$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  private$.properties$support <- Set$new(0:self$getParameterValue("size"))
  if(self$getParameterValue("prob") == 0.5)
    private$.properties$symmetry <- "asymmetric"
  else
    private$.properties$symmetry <- "symmetric"
  invisible(self)
})

Binomial$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
  if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
  if(!is.null(paramlst$qprob)) lst = c(lst, list(prob = 1-paramlst$qprob))
  return(lst)
})
Binomial$set("private", ".pdf", function(x, log){
  if(checkmate::testList(self$getParameterValue("size"))){
    mapply(dbinom, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
           MoreArgs = list(x = x, log = log))
  } else {
    dbinom(x, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"), log = log)
  }
})
Binomial$set("private", ".cdf", function(x, lower.tail, log.p){
  if (checkmate::testList(self$getParameterValue("size"))) {
    mapply(pbinom, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
      MoreArgs = list(x = x, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    pbinom(x, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
           lower.tail = lower.tail, log.p = log.p)
  }
})
Binomial$set("private", ".quantile", function(p, lower.tail, log.p){
  if (checkmate::testList(self$getParameterValue("size"))) {
    mapply(qbinom, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
      MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
    )
  } else {
    qbinom(p, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
      lower.tail = lower.tail, log.p = log.p)
  }
})
Binomial$set("private", ".rand", function(n){
  if (checkmate::testList(self$getParameterValue("size"))) {
    mapply(rbinom, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"),
      MoreArgs = list(n = n)
    )
  } else {
    rbinom(n, size = self$getParameterValue("size"), prob = self$getParameterValue("prob"))
  }
})
Binomial$set("private", ".log", TRUE)

Binomial$set("public","initialize",function(size = 10, prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, size, prob, qprob, verbose)
  self$setParameterValue(size = size, prob = prob, qprob = qprob)

  super$initialize(decorators = decorators,
                   support = Set$new(0:size, class = "integer"),
                   type = Naturals$new(),
                   symmetry = if(prob == 0.5) "symmetric" else "asymmetric",
                   valueSupport = "discrete",
                   variateForm = "univariate")
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Binom", ClassName = "Binomial",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
