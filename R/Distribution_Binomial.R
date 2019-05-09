#' @title Binomial Distribution
#'
#' @include SetInterval_SpecialSet.R ParameterSet.R
#' @export
Binomial <- R6::R6Class("Binomial", inherit = Distribution, lock_objects = F)
Binomial$set("public","name","Binomial")
Binomial$set("public","short_name","Binom")
Binomial$set("public","traits",list(type = PosIntegers$new(zero = T),
                                    valueSupport = "discrete",
                                    variateForm = "univariate"))

Binomial$set("public","properties",list())

Binomial$set("public","pdf",function(x, log = FALSE)
  dbinom(x, self$getParameterValue("size"), self$getParameterValue("prob"),log))

Binomial$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE)
  pbinom(q, self$getParameterValue("size"), self$getParameterValue("prob"), lower.tail, log.p))

Binomial$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE)
  qbinom(p, self$getParameterValue("size"), self$getParameterValue("prob"),log.p))

Binomial$set("public","rand",function(n)
  rbinom(n, self$getParameterValue("size"), self$getParameterValue("prob")))

Binomial$set("public","expectation",function()
  self$getParameterValue("size") * self$getParameterValue("prob"))

Binomial$set("public","var",function()
  self$getParameterValue("size") * self$getParameterValue("prob") * self$getParameterValue("qprob"))

Binomial$set("public","skewness",function()
  (1 - (2*self$getParameterValue("prob"))) / self$sd())

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

Binomial$set("public","survival",function(q, log.p = FALSE)
  self$cdf(q, lower.tail = FALSE, log.p))

Binomial$set("public","hazard",function(x)
  self$pdf(x)/self$survival(x))

Binomial$set("public","cumhazard",function(x)
  -self$cdf(x, log.p = TRUE))

Binomial$set("private",".parameters",
  ParameterSet$new(id = list("prob","size","qprob"), value = list(0.5, 10, 0.5),
                   lower = list(0, 1, 0), upper = list(1, Inf, 1),
                   class = list("numeric","integer","numeric"),
                   settable = list(TRUE, TRUE, FALSE), fittable = list(TRUE, FALSE, FALSE),
                   updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                   description = list("Probability of Success", "Number of trials",
                                      "Probability of failure"))
  )

Binomial$set("public","initialize",function(size = 10, prob = 0.5, decorators = NULL){

  self$setParameterValue(list(size = size, prob = prob))

  unlockBinding("properties", self)
  self$properties$support <- Set$new(0:size)

  self$properties$distrDomain = PosIntegers$new(zero = T)

  if(prob == 0.5 | size >= 30)
    self$properties$symmetry <- "symmetric"
  else
    self$properties$symmetry <- "asymmetric"
  lockBinding("properties", self)

  super$initialize(decorators = decorators)
  invisible(self)
})