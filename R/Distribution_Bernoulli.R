#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Bernoulli Distribution Documentation
#-------------------------------------------------------------
#' @title Bernoulli Distribution
#'
#' @description Mathematical and statistical functions for the Bernoulli distribution parameterised
#' with probability of success.
#'
#' @name Bernoulli
#'
#' @section Constructor: Bernoulli$new(prob = 0.5, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' }
#'
#' @section Constructor Details: The Bernoulli distribution is parameterised with prob (probability of
#' success) as a number between 0 and 1.
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
# Bernoulli Distribution Definition
#-------------------------------------------------------------
Bernoulli <- R6::R6Class("Bernoulli", inherit = Distribution, lock_objects = F)
Bernoulli$set("public","name","Bernoulli")
Bernoulli$set("public","short_name","Bern")
Bernoulli$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))
Bernoulli$set("public","description","Bernoulli Probability Distribution.")

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
  (-self$getParameterValue("qprob")*log(self$getParameterValue("qprob"))) +
    (-self$getParameterValue("prob")*log(self$getParameterValue("prob")))
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

Bernoulli$set("public","initialize",function(prob = 0.5, decorators = NULL){

  private$.parameters <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                                          lower = list(0, 0), upper = list(1, 1),
                                          class = list("numeric","numeric"),
                                          settable = list(TRUE, FALSE),
                                          updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                                          description = list("Probability of Success",
                                                             "Probability of failure"))

  self$setParameterValue(list(prob = prob))

  pdf = function(x1) dbinom(x1, 1, self$getParameterValue("prob"))
  cdf = function(x1) pbinom(x1, 1, self$getParameterValue("prob"))
  quantile = function(p) qbinom(p, 1, self$getParameterValue("prob"))
  rand = function(n) dbinom(n, 1, self$getParameterValue("prob"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0,1), distrDomain = PosIntegers$new(zero = T),
                   symmetric = FALSE)
  invisible(self)
})
