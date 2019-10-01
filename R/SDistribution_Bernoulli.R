#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Bernoulli Distribution Documentation
#-------------------------------------------------------------
#' @name Bernoulli
#' @template SDist
#' @templateVar ClassName Bernoulli
#' @templateVar DistName Bernoulli
#' @templateVar uses to model a two-outcome scenario
#' @templateVar params probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = p, \ if \ x = 1}{f(x) = p, if x = 1}\deqn{f(x) = 1 - p, \ if \ x = 0}{f(x) = 1 - p, if x = 0}
#' @templateVar paramsupport \eqn{p \ \in \ [0,1]}{p \epsilon [0,1]}
#' @templateVar distsupport \eqn{\{0,1\}}{{0,1}}
#' @templateVar constructor prob = 0.5, qprob = NULL
#' @templateVar arg1 \code{prob} \tab numeric \tab probability of success. \cr
#' @templateVar arg2 \code{qprob} \tab numeric \tab probability of failure. \cr
#' @templateVar constructorDets \code{prob} or \code{qprob} as a number between 0 and 1. These are related via, \deqn{qprob = 1 - prob} If \code{qprob} is given then {prob is ignored}.
#' @templateVar additionalSeeAlso \code{\link{Binomial}} for a generalisation of the Bernoulli distribution.
#'
#' @examples
#' # Can be parameterised with probability of success or failure
#' Bernoulli$new(prob = 0.2)
#' Bernoulli$new(qprob = 0.3)
#'
#' x = Bernoulli$new(verbose = TRUE) # Default is with prob = 0.5
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(qprob = 0.3)
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
# Bernoulli Distribution Definition
#-------------------------------------------------------------
Bernoulli <- R6::R6Class("Bernoulli", inherit = SDistribution, lock_objects = F)
Bernoulli$set("public","name","Bernoulli")
Bernoulli$set("public","short_name","Bern")
Bernoulli$set("public","description","Bernoulli Probability Distribution.")
Bernoulli$set("public","package","stats")

Bernoulli$set("public","mean",function(){
  self$getParameterValue("prob")
})
Bernoulli$set("public","variance",function(){
  self$getParameterValue("prob") * self$getParameterValue("qprob")
})
Bernoulli$set("public","skewness",function(){
  (1 - (2*self$getParameterValue("prob"))) / self$stdev()
})
Bernoulli$set("public","kurtosis",function(excess = TRUE){
  exkurtosis = (1 - (6*self$getParameterValue("prob") * self$getParameterValue("qprob"))) / self$variance()
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
Bernoulli$set("public","entropy",function(base = 2){
  (-self$getParameterValue("qprob")*log(self$getParameterValue("qprob"), base)) +
    (-self$getParameterValue("prob")*log(self$getParameterValue("prob"), base))
})
Bernoulli$set("public", "mgf", function(t){
  return(self$getParameterValue("qprob") + (self$getParameterValue("prob") * exp(t)))
})
Bernoulli$set("public", "cf", function(t){
  return(self$getParameterValue("qprob") + (self$getParameterValue("prob") * exp(1i*t)))
})
Bernoulli$set("public","pgf",function(z){
  return(self$getParameterValue("qprob") + (self$getParameterValue("prob") * z))
})
Bernoulli$set("public","mode",function(which = "all"){
  if(self$getParameterValue("prob") < 0.5)
    return(0)
  else if(self$getParameterValue("prob") > 0.5)
    return(1)
  else{
    if(which == "all")
      return(c(0,1))
    else
      return(c(0,1)[which])
  }
})

Bernoulli$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
  else if(!is.null(paramlst$qprob)) lst = c(lst, list(prob = 1-paramlst$qprob))
  return(lst)
})


Bernoulli$set("public","initialize",function(prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, prob, qprob, verbose)
  if(!is.null(qprob)) prob <- NULL
  self$setParameterValue(prob = prob, qprob = qprob)

  pdf = function(x1) dbinom(x1, 1, self$getParameterValue("prob"))
  cdf = function(x1) pbinom(x1, 1, self$getParameterValue("prob"))
  quantile = function(p) qbinom(p, 1, self$getParameterValue("prob"))
  rand = function(n) dbinom(n, 1, self$getParameterValue("prob"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0,1),
                   symmetric = FALSE,type = Naturals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Bern", ClassName = "Bernoulli",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))

