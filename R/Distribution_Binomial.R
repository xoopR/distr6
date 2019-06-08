#-------------------------------------------------------------
# Binomial Distribution Documentation
#-------------------------------------------------------------
#' @title Binomial Distribution
#'
#' @description Mathematical and statistical functions for the Binomial distribution parameterised
#' with probability of success and size (number of trials).
#'
#' @name Binomial
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab number of trials, default = 10. \cr
#' \code{prob} \tab numeric \tab probability of success, default = 0.5. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' }
#'
#' @section Constructor Details: The Binomial distribution is parameterised with size (number of trials)
#' as an integer and prob (probability of success) as a number between 0 and 1.
#'
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#'
#' @section Public Methods:
#'  \tabular{lrr}{
#'   \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#'   \code{pdf(x1, log = FALSE)} \tab character \tab Evaluates density at x1. \cr
#'   \code{cdf(x1, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evaluates distribution function at x1. \cr
#'   \code{quantile(p, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evalutes inverse distribution at p.  \cr
#'   \code{rand(n)} \tab numeric \tab Randomly generates n samples from the distribution.  \cr
#'   \code{expectation()} \tab numeric \tab Expectation.  \cr
#'   \code{var()} \tab numeric \tab Variance.  \cr
#'   \code{skewness()} \tab numeric \tab Skewness. \cr
#'   \code{kurtosis(excess = TRUE)} \tab numeric \tab Kurtosis. Kurtosis - 3 if excess = TRUE. \cr
#'   \code{entropy(base = 2)} \tab numeric \tab Entropy. Shannon if base = 2. \cr
#'   \code{mgf(t)} \tab numeric \tab Evaluates moment generating function at t. \cr
#'   \code{pgf(t)} \tab numeric \tab Evaluates probability generating function at t. \cr
#'   \code{cf(t)} \tab numeric \tab Evaluates characteristic function at t. \cr
#'   \code{survival(x1, log.p = FALSE)} \tab numeric \tab Evaluates survival function at x1. \cr
#'   \code{hazard(x1)} \tab numeric \tab Evaluates hazard function at t. \cr
#'   \code{cumHazard(x1)} \tab numeric \tab Evaluates cumulative hazard function at t. \cr
#'   }
#'
NULL
#-------------------------------------------------------------
# Binomial Distribution Definition
#-------------------------------------------------------------
#' @include SetInterval_SpecialSet.R ParameterSet.R
#' @export
Binomial <- R6::R6Class("Binomial", inherit = Distribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))

Binomial$set("public","expectation",function(){
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

Binomial$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1 = x1, lower.tail = FALSE, log.p = log.p)
})

Binomial$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

Binomial$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})

Binomial$set("public","setParameterValue",function(lst){
  super$setParameterValue(lst)
  private$.properties$support <- Set$new(0:self$getParameterValue("size"))
})

Binomial$set("private",".parameters", NULL)

Binomial$set("public","initialize",function(size = 10, prob = 0.5, decorators = NULL){

  private$.parameters <- ParameterSet$new(id = list("prob","size","qprob"), value = list(0.5, 10, 0.5),
                                          lower = list(0, 1, 0), upper = list(1, Inf, 1),
                                          class = list("numeric","integer","numeric"),
                                          settable = list(TRUE, TRUE, FALSE),
                                          updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                                          description = list("Probability of Success", "Number of trials",
                                                             "Probability of failure"))

  self$setParameterValue(list(size = size, prob = prob))

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
