#' @include SetInterval_SpecialSet.R ParameterSet.R
Exponential <- R6::R6Class("Exponential", inherit = Distribution, lock_objects = F)
Exponential$set("public","name","Exponential")
Exponential$set("public","short_name","Exp")
Exponential$set("public","traits",list(type = PosReals$new(zero = T),
                                    valueSupport = "continuous",
                                    variateForm = "univariate"))

Exponential$set("public","properties",list(support = PosReals$new(zero = T),
                                           distrDomain = PosReals$new(zero = T),
                                           symmetry  = "asymmetric"))

Exponential$set("public","pdf",function(x, log = FALSE)
  dexp(x, self$getParameterValue("rate"), log))

Exponential$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE)
  pexp(q, self$getParameterValue("rate"), lower.tail, log.p))

Exponential$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE)
  qexp(p, self$getParameterValue("rate"), log.p))

Exponential$set("public","rand",function(n)
  rexp(n, self$getParameterValue("rate")))

Exponential$set("public","expectation",function()
  self$getParameterValue("scale"))

Exponential$set("public","var",function()
  self$getParameterValue("scale")^2)

Exponential$set("public","skewness",function() return(2))

Exponential$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(6)
  else
    return(9)
})

Exponential$set("public","entropy",function(base = 2){
  1 - log(self$getParameterValue("rate"), base)
})

Exponential$set("public", "mgf", function(t){
  if(t < self$getParameterValue("rate"))
    return(self$getParameterValue("rate") / (self$getParameterValue("rate") - t))
  else
    return(0)
})

Exponential$set("public", "cf", function(t){
  return(self$getParameterValue("rate") / (self$getParameterValue("rate") -  ((0+1i) * t)))
})

Exponential$set("public","survival",function(q, log.p = FALSE)
  self$cdf(q, lower.tail = FALSE, log.p))

Exponential$set("public","hazard",function(x)
  self$pdf(x)/self$survival(x))

Exponential$set("public","cumhazard",function(x)
  -self$cdf(x, log.p = TRUE))

Exponential$set("private",".parameters",
             ParameterSet$new(id = list("rate","scale"), value = list(1, 1),
                              lower = list(0, 0), upper = list(Inf, Inf),
                              class = list("numeric","numeric"),
                              settable = list(TRUE, FALSE), fittable = list(TRUE, FALSE),
                              updateFunc = list(NULL, "1 / self$getParameterValue('rate')"),
                              description = list("Arrival Rate", "Scale"))
)

Exponential$set("public","initialize",function(rate = 1, decorators = NULL){

  self$setParameterValue(list(rate = rate))

  super$initialize(decorators = decorators)
  invisible(self)
})
