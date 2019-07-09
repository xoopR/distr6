#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Binomial Distribution Documentation
#-------------------------------------------------------------
#' @title Binomial Distribution Class
#'
#' @description Mathematical and statistical functions for the Binomial distribution, which is commonly
#' used to model the number of successes out of a number of independent trials.
#'
#' @details The Binomial distribution parameterised with number of trials, n, and probability of
#' success, p, is defined by the pmf,
#' \deqn{f(x) = C(n, x)p^x(1-p)^{n-x}}
#' for \eqn{n = 0,1,2,\ldots} and \eqn{p \epsilon [0,1]}, where \eqn{C(a,b)} is the combination
#' (or binomial coefficient) function.
#'
#' The distribution is supported on \eqn{{0, 1,...,n}}.
#'
#' @name Binomial
#'
#' @section Constructor: Binomial$new(size = 10, prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab number of trials. \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{qprob} \tab numeric \tab probability of failure. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Binomial distribution is parameterised with \code{size} as a whole number,
#' and either \code{prob} or \code{qprob} as a number between 0 and 1. These are related via,
#' \deqn{qprob = 1 - prob}
#' If \code{qprob} is given then \code{prob} is ignored.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Can be parameterised with probability of success or failure
#' Binomial$new(prob = 0.2)
#' Binomial$new(qprob = 0.3)
#'
#' x = Binomial$new() # Default is with prob = 0.5 and size = 10
#'
#' # Update parameters
#' x$setParameterValue(list(size = 4, qprob = 0.1)) # Can update any parameter
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
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Binomial Distribution Definition
#-------------------------------------------------------------
Binomial <- R6::R6Class("Binomial", inherit = SDistribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","description","Binomial Probability Distribution.")
Binomial$set("public","package","stats")

Binomial$set("public","mean",function(){
  self$getParameterValue("size") * self$getParameterValue("prob")
})
Binomial$set("public","mode",function(){
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

Binomial$set("public","setParameterValue",function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  private$.properties$support <- Set$new(0:self$getParameterValue("size"))
  if(self$getParameterValue("prob")==0.5)
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


Binomial$set("public","initialize",function(size = 10, prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, size, prob, qprob, verbose)
  self$setParameterValue(list(size = size, prob = prob, qprob = qprob))

  if(prob == 0.5)
    symmetric <- TRUE
  else
    symmetric <- FALSE

  pdf = function(x1) dbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  cdf = function(x1) pbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  quantile = function(p) qbinom(p, self$getParameterValue("size"), self$getParameterValue("prob"))
  rand = function(n) rbinom(n, self$getParameterValue("size"), self$getParameterValue("prob"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0:size), distrDomain = Naturals$new(),
                   symmetric = symmetric,type = Naturals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})
