#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Bernoulli Distribution Documentation
#-------------------------------------------------------------
#' @title Bernoulli Distribution
#'
#' @description Mathematical and statistical functions for the Bernoulli distribution parameterised
#' with prob or \eqn{qprob = 1 - prob}. The prob parameterisation is defined by the pmf,
#' \deqn{f(x) = p, if x =1; 1-p, if x = 0}
#'
#' where \eqn{p \epsilon [0,1]} is the prob parameter.
#'
#' @details The default parameterisation of probability of success is favoured over the probability
#' of failure as this is more common in practice, however the two are mathematically identical (subject to
#' a simple translation).
#'
#' @name Bernoulli
#'
#' @section Constructor: Bernoulli$new(prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{qprob} \tab numeric \tab probability of failure. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Bernoulli distribution is parameterised with prob (probability of
#' success) or qprob (probability of failure) as a number between 0 and 1. If \code{qprob} is given then
#' \code{prob} is ignored.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Can be parameterised with probability of success or failure
#' Bernoulli$new(prob = 0.2)
#' Bernoulli$new(qprob = 0.3)
#'
#' x = Bernoulli$new(verbose = TRUE) # Default is with prob = 0.5
#'
#' # Update parameters
#' x$setParameterValue(list(qprob = 0.3)) # Can update any parameter
#' x$parameters()
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
# Bernoulli Distribution Definition
#-------------------------------------------------------------
Bernoulli <- R6::R6Class("Bernoulli", inherit = SDistribution, lock_objects = F)
Bernoulli$set("public","name","Bernoulli")
Bernoulli$set("public","short_name","Bern")
Bernoulli$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))
Bernoulli$set("public","description","Bernoulli Probability Distribution.")
Bernoulli$set("public","package","distr6")

Bernoulli$set("public","mean",function(){
  self$getParameterValue("prob")
})
Bernoulli$set("public","var",function(){
  self$getParameterValue("prob") * self$getParameterValue("qprob")
})
Bernoulli$set("public","skewness",function(){
  (1 - (2*self$getParameterValue("prob"))) / self$sd()
})
Bernoulli$set("public","kurtosis",function(excess = TRUE){
  exkurtosis = (1 - (6*self$getParameterValue("prob") * self$getParameterValue("qprob"))) / self$var()
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
  self$setParameterValue(list(prob = prob, qprob = qprob))

  pdf = function(x1) dbinom(x1, 1, self$getParameterValue("prob"))
  cdf = function(x1) pbinom(x1, 1, self$getParameterValue("prob"))
  quantile = function(p) qbinom(p, 1, self$getParameterValue("prob"))
  rand = function(n) dbinom(n, 1, self$getParameterValue("prob"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0,1), distrDomain = PosIntegers$new(zero = T),
                   symmetric = FALSE)
  invisible(self)
})
