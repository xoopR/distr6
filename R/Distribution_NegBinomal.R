#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Negative Binomial Distribution Documentation
#-------------------------------------------------------------
#' @title Negative Binomial Distribution
#'
#' @description Mathematical and statistical functions for the Negative Binomial distribution
#' parameterised with probability of success and size (number of successes).
#'
#' @name NegBinomial
#'
#' @section Constructor: NegBinomial$new(size = 1, prob = 0.5, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab target number of successes. \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{qprob} \tab numeric \tab probability of failure. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
##' @section Constructor Details: 
#' The Negative Binomial distribution with size = n (positive) and
#'  prob = p (0 <= p <= 1) has density function defined as
#' \deqn{f(x) = (x + n - 1)C(n - 1) p^n (1 - p)^x},
#' where n is the number of successes (size), p is the probability of each 
#' success (prob) and x is interpreted as the number of failures before 
#' the n-th success.If \code{qprob} is given then \code{prob} is ignored. This quantifies
#' the probability of having x failuers in a sequence of independent and identical 
#' Bernoulli before the n-th success is obtained.
#' Compared with a Binomial distribution, which counts the number of successes in a 
#' fixed number of trials, a Negative Binomial distribution pre-specifies the target
#' number of successes and counts the number of failures required to reach this 
#' target number. A NegBinomial(1,p) is the same as a Geometric(p).
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#' @example 
#' NegBinom <- NegBinomial$new(size = 10, prob = 0.5)
#' @export
NULL

#-------------------------------------------------------------
# NegBinomial Distribution Definition
#-------------------------------------------------------------
NegBinomial <- R6::R6Class("NegBinomial", inherit = SDistribution, lock_objects = F)
NegBinomial$set("public", "name", "NegBinomial")
NegBinomial$set("public", "shortname", "NBinom")
NegBinomial$set("public", "traits", list(type = PosIntegers$new(zero = T),
                                         valueSupport = "discrete",
                                         variateForm = "univariate"))
NegBinomial$set("public","description","Negative Binomial Probability Distribution.")

NegBinomial$set("public", "mean", function(){
  self$getParameterValue("size") * self$getParameterValue("qprob") / self$getParameterValue("prob")
})

NegBinomial$set("public","var",function(){
  self$getParameterValue("size") * self$getParameterValue("qprob") / (self$getParameterValue("prob")^2)
})

NegBinomial$set("public", "skewness", function(){
  (2 - self$getParameterValue("prob")) / sqrt(self$getParameterValue("size") * self$getParameterValue("qprob"))
})

NegBinomial$set("public", "kurtosis", function(excess = TRUE){
  exkurtosis = (self$getParameterValue("prob")^2 - 6*self$getParameterValue("prob") + 6)/
    (self$getParameterValue("size") * self$getParameterValue("qprob"))
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})

NegBinomial$set("public", "mgf", function(t){
  self$getParameterValue("prob")^self$getParameterValue("size") * (1 - self$getParameterValue("qprob")*exp(t))^(-self$getParameterValue("size"))
})

NegBinomial$set("public", "cf", function(t){
  P <- (1 - self$getParameterValue("prob"))/self$getParameterValue("prob")
  Q <- 1 / self$getParameterValue("prob")
  (Q - P*exp((0+1i) * t))^(-self$getParameterValue("size"))
})  

NegBinomial$set("public", "pgf", function(z){
  ((self$getParameterValue("prob")*z) / (1 - self$getParameterValue("qrob")*z))^self$getParameterValue("size")
})


NegBinomial$set("public","setParameterValue",function(lst, error = "warn"){
   super$setParameterValue(lst, error)
   private$.properties$support <- Set$new(0:self$getParameterValue("size"))
 })

NegBinomial$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
  if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
  if(!is.null(paramlst$qprob)) lst = c(lst, list(prob = 1-paramlst$qprob))
  return(lst)
})


NegBinomial$set("public","initialize", function(size = 1, prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE){
  
  private$.paramaters <- getParameterSet(self, size, prob, qprob, verbose)
  self$setParameterValue(list(size = size, prob = prob, qprob = qprob))
  
  if(size >= 30)
    symmetric <- TRUE
  else
    symmetric <- FALSE
  
  pdf = function(x1) dnbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  cdf = function(x1) pnbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  quantile = function(x1) qnbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  rand = function(x1) rnbinom(n, self$getParameterValue("size"), self$getParameterValue("prob"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0:size), distrDomain = PosIntegers$new(zero = T),
                   symmetric = symmetric)
  invisible(self)
})


