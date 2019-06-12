#' @include SetInterval_SpecialSet.R ParameterSet.R
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
#' @section Constructor: Binomial$new(size = 10, prob = 0.5, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab number of trials. \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' }
#'
#' @section Constructor Details: The Binomial distribution is parameterised with size (number of trials)
#' as an integer and prob (probability of success) as a number between 0 and 1.
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
Binomial <- R6::R6Class("Binomial", inherit = Distribution, lock_objects = F)
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
