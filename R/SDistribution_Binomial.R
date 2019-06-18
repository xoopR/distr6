#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Binomial Distribution Documentation
#-------------------------------------------------------------
#' @title Binomial Distribution
#'
#' @description Mathematical and statistical functions for the Binomial distribution parameterised
#' with size (number of trials) and prob or \eqn{qprob = 1 - prob}. The size-prob Binomial distribution
#' is defined by the pmf,
#' \deqn{f(x) = C(n, x)p^x(1-p)^(n-x)}
#' where \eqn{n = 0,1,2,\ldots} is the size parameter, \eqn{p \epsilon [0,1]} is the prob parameter and
#' \eqn{C(a,b)} is the combination (or binomial coefficient) function.
#'
#' @details The default parameterisation of number of trials and probability of success is favoured
#' over the probability of failure as talking in terms of failures is more commonly associated with
#' the negative binomial distribution.
#'
#' @name Binomial
#'
#' @section Constructor: Binomial$new(size = 10, prob = 0.5, decorators = NULL, verbose = FALSE)
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
#' @section Constructor Details: The Binomial distribution is parameterised with size (number of trials)
#' as an integer and either prob (probability of success) or qprob (probability of failure) as a number
#' between 0 and 1. If \code{qprob} is given then \code{prob} is ignored.
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
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#'
#' @export
NULL
#-------------------------------------------------------------
# Binomial Distribution Definition
#-------------------------------------------------------------
Binomial <- R6::R6Class("Binomial", inherit = SDistribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))
Binomial$set("public","description","Binomial Probability Distribution.")

Binomial$set("public","mean",function(){
  self$getParameterValue("size") * self$getParameterValue("prob")
})
Binomial$set("public","var",function(){
  self$getParameterValue("size") * self$getParameterValue("prob") * self$getParameterValue("qprob")
})
Binomial$set("public","skewness",function(){
  (1 - (2*self$getParameterValue("prob"))) / self$sd()
})
Binomial$set("public","kurtosis",function(excess = TRUE){
  exkurtosis = (1 - (6*self$getParameterValue("prob") * self$getParameterValue("qprob"))) / self$var()
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
Binomial$set("public","entropy",function(base = 2){
  0.5 * log(2 * pi * exp(1) * self$var(), base)
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

  if(prob == 0.5 | size >= 30)
    symmetric <- TRUE
  else
    symmetric <- FALSE

  pdf = function(x1) dbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  cdf = function(x1) pbinom(x1, self$getParameterValue("size"), self$getParameterValue("prob"))
  quantile = function(p) qbinom(p, self$getParameterValue("size"), self$getParameterValue("prob"))
  rand = function(n) dbinom(n, self$getParameterValue("size"), self$getParameterValue("prob"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0:size), distrDomain = PosIntegers$new(zero = T),
                   symmetric = symmetric)
  invisible(self)
})
