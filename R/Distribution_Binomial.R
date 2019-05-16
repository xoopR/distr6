#' @title Binomial Distribution
#' @description Mathematical and statistical functions for the Binomial distribution parameterised
#' with probability of success and size (number of trials).
#' @name Binomial
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab number of trials, default = 10. \cr
#' \code{prob} \tab numeric \tab probability of success, default = 0.5. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{...} \tab ANY \tab additional arguments for Distribution constructor. See details. \cr
#' }
#'
#' @section Constructor Details: The Binomial distribution is parameterised with size (number of trials)
#' as an integer and prob (probability of success) as a number between 0 and 1.
#'
#' The CoreStatistics and ExoticStatistics decorators can be added to the distribution for further
#' numeric functionality, but these are approximate calculations only. Additional arguments can be passed
#' to the Distribution constructor, including R62S3 to determine if S3 methods should be added for
#' the Binomial distribution.
#'
#'
#' @section Public Variables:
#'  \tabular{lr}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab "Binomial" \cr
#'   \code{short_name} \tab "Binom" \cr
#'   \code{traits} \tab List of Binomial distribution traits. \cr
#'   \code{properties} \tab List of Binomial distribution properties. \cr
#'   }
#'
#' @section Public Methods:
#'  \tabular{lrr}{
#'   \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#'   \code{pdf(x, log = FALSE)} \tab character \tab Evaluates density at x. \cr
#'   \code{cdf(q, lower.tail = TRUE, log.p = FALSE)} \tab numeric \tab Evaluates distribution function at q. \cr
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
#'   \code{survival(q, log.p = FALSE)} \tab numeric \tab Evaluates survival function at q. \cr
#'   \code{hazard(x)} \tab numeric \tab Evaluates hazard function at t. \cr
#'   \code{cumHazard(x)} \tab numeric \tab Evaluates cumulative hazard function at t. \cr
#'   }
#'
#' @section Public Methods Details:
#' If \code{log.p} is TRUE then the natural logarithm of probabilities is returned. If \code{lower.tail}
#' is TRUE then distribution functions are evaluated at the lower tail of the distribution, otherwise
#' the upper tail (1 - p).
#'
#'
#' @seealso See \code{\link{Distribution}} for inherited methods and variables. See \code{\link{DistributionDecorator}}
#' for Decorator details as well as \code{\link{CoreStatistics}} and \code{\link{ExoticStatistics}}.
NULL

#' @include SetInterval_SpecialSet.R ParameterSet.R
#' @export
Binomial <- R6::R6Class("Binomial", inherit = Distribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))

Binomial$set("public","properties",list())

Binomial$set("private",".pdf",function(x){
  dbinom(x, self$getParameterValue("size"), self$getParameterValue("prob"))
})

Binomial$set("private",".cdf",function(q){
  pbinom(q, self$getParameterValue("size"), self$getParameterValue("prob"))
})

Binomial$set("private",".quantile",function(p){
  qbinom(p, self$getParameterValue("size"), self$getParameterValue("prob"))
})

Binomial$set("private",".rand",function(n){
  rbinom(n, self$getParameterValue("size"), self$getParameterValue("prob"))
})

Binomial$set("public","expectation",function()
  self$getParameterValue("size") * self$getParameterValue("prob"))

Binomial$set("public","var",function()
  self$getParameterValue("size") * self$getParameterValue("prob") * self$getParameterValue("qprob"))

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

Binomial$set("public","survival",function(q, log.p = FALSE){
  self$cdf(q = q, lower.tail = FALSE, log.p = log.p)
})

Binomial$set("public","hazard",function(x){
  self$pdf(x)/self$survival(x)
})

Binomial$set("public","cumHazard",function(x){
  -self$cdf(x, log.p = TRUE)
})

Binomial$set("public","setParameterValue",function(lst){
  super$setParameterValue(lst)
  unlockBinding("properties", self)
  self$properties$support <- Set$new(0:self$getParameterValue("size"))
  lockBinding("properties", self)
})

Binomial$set("private",".parameters", NULL)
Binomial$set("private",".setSupport", function(support){

})

Binomial$set("public","initialize",function(size = 10, prob = 0.5, decorators = NULL, ...){

  private$.parameters <- ParameterSet$new(id = list("prob","size","qprob"), value = list(0.5, 10, 0.5),
                                          lower = list(0, 1, 0), upper = list(1, Inf, 1),
                                          class = list("numeric","integer","numeric"),
                                          settable = list(TRUE, TRUE, FALSE), fittable = list(TRUE, FALSE, FALSE),
                                          updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                                          description = list("Probability of Success", "Number of trials",
                                                             "Probability of failure"))

  self$setParameterValue(list(size = size, prob = prob))

  unlockBinding("properties", self)
  self$properties$support <- Set$new(0:size)

  self$properties$distrDomain = PosIntegers$new(zero = T)

  if(prob == 0.5 | size >= 30)
    self$properties$symmetry <- "symmetric"
  else
    self$properties$symmetry <- "asymmetric"
  lockBinding("properties", self)

  super$initialize(decorators = decorators, ...)
  invisible(self)
})
