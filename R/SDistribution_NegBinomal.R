#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Negative Binomial Distribution Documentation
#-------------------------------------------------------------
#' @title Negative Binomial Distribution
#'
#' @description Mathematical and statistical functions for the Negative Binomial distribution parameterised
#' with size and one of: prob, \eqn{qprob = 1 - prob} or mean (formula dependent on parameterisation, see details.)
#' The Negative Binomial determining the number of success before \eqn{n} failures is defined by the pmf,
#' \deqn{f(x) = C(x + n - 1, n) p^n (1 - p)^n}
#' where \eqn{n = 0,1,2,\ldots} is the size parameter, \eqn{p \epsilon [0,1]} is the prob parameter and
#' \eqn{C(a,b)} is the combination (or binomial coefficient) function.
#'
#' @details The Negative Binomial distribution can refer to one of four distributions:
#'
#' 1. The number of successes before K failures
#'
#' 2. The number of trials before K failures
#'
#' 3. The number of failures before K successes
#'
#' 4. The number of trials before K successes
#'
#' For each we refer to the number of K successs/failures as the \code{size} parameter, \code{prob}
#' is always the probability of success and \code{qprob} is the probability of failure.
#'
#' @name NegativeBinomial
#'
#' @section Constructor: NegativeBinomial$new(size = 10, prob = 0.5, qprob = NULL, type = "sbf", decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab number of failures. \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{qprob} \tab numeric \tab probability of failure. \cr
#' \code{type} \tab character \tab type of negative binomial, see details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Negative Binomial distribution is parameterised with size,
#' as an integer and either prob (probability of success) or qprob (probability of failure) as a number
#' between 0 and 1. If \code{qprob} is given then \code{prob} is ignored. The additional parameter
#' \code{type} determines which of the four Negative Binomial distributions should be constructed, this
#' cannot be updated after construction. \code{type} should be one of "sbf" (successes before failures),
#' "tbf" (trials before failures), "fbs" (failures before successes) or "tbs" (trials before successes).
#'
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#
#
#' @export
NULL
#-------------------------------------------------------------
# NegativeBinomial Distribution Definition
#-------------------------------------------------------------
NegativeBinomial <- R6::R6Class("NegativeBinomial", inherit = SDistribution, lock_objects = F)
NegativeBinomial$set("public", "name", "NegativeBinomial")
NegativeBinomial$set("public", "short_name", "NBinom")
NegativeBinomial$set("public", "traits", list(type = PosIntegers$new(zero = T),
                                         valueSupport = "discrete",
                                         variateForm = "univariate"))
NegativeBinomial$set("private",".type",NULL)
NegativeBinomial$set("public","description","Negative Binomial Probability Distribution.")
NegativeBinomial$set("public","package","distr6")

NegativeBinomial$set("public", "mean", function(){
  self$getParameterValue("size") * self$getParameterValue("qprob") / self$getParameterValue("prob")
})

NegativeBinomial$set("public","var",function(){
  self$getParameterValue("size") * self$getParameterValue("qprob") / (self$getParameterValue("prob")^2)
})

NegativeBinomial$set("public", "skewness", function(){
  (2 - self$getParameterValue("prob")) / sqrt(self$getParameterValue("size") * self$getParameterValue("qprob"))
})

NegativeBinomial$set("public", "kurtosis", function(excess = TRUE){
  exkurtosis = (self$getParameterValue("prob")^2 - 6*self$getParameterValue("prob") + 6)/
    (self$getParameterValue("size") * self$getParameterValue("qprob"))
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})

NegativeBinomial$set("public", "mgf", function(t){
  self$getParameterValue("prob")^self$getParameterValue("size") * (1 - self$getParameterValue("qprob")*exp(t))^(-self$getParameterValue("size"))
})

NegativeBinomial$set("public", "cf", function(t){
  P <- (1 - self$getParameterValue("prob"))/self$getParameterValue("prob")
  Q <- 1 / self$getParameterValue("prob")
  (Q - P*exp((0+1i) * t))^(-self$getParameterValue("size"))
})

NegativeBinomial$set("public", "pgf", function(z){
  ((self$getParameterValue("prob")*z) / (1 - self$getParameterValue("qrob")*z))^self$getParameterValue("size")
})


NegativeBinomial$set("public","setParameterValue",function(lst, error = "warn"){
   super$setParameterValue(lst, error)
   private$.properties$support <- Set$new(0:self$getParameterValue("size"))
 })

NegativeBinomial$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
  if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
  if(!is.null(paramlst$qprob)) lst = c(lst, list(prob = 1-paramlst$qprob))
  return(lst)
})


NegativeBinomial$set("public","initialize", function(size = 1, prob = 0.5, qprob = NULL, decorators = NULL, verbose = FALSE){

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


