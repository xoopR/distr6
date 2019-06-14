library(distr6)
Poisson <- R6::R6Class("Poisson", inherit = Distribution, lock_objects = F)
Poisson$set("public","name","Poisson")
Poisson$set("public","short_name","Pois")
Poisson$set("public","traits",list(type = PosIntegers$new(zero = T),
                                   valueSupport = "discrete",
                                   variateForm = "univariate"))


Poisson$set("public","mean",function(){
  self$getParameterValue("rate")
})

Poisson$set("public","var",function(){
  self$getParameterValue("rate") 
})

Poisson$set("public","skewness",function(){
  (self$getParameterValue("rate"))^(-1/2)
})

Poisson$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(1/self$getParameterValue("rate"))
  else
    return(1/self$getParameterValue("rate") + 3)
})


Poisson$set("public", "mgf", function(t){
  exp(self$getParameterValue("rate")*(exp(t)-1))
})

Poisson$set("public", "cf", function(t){
  exp(self$getParameterValue("rate")*(exp(1i*t)-1))
})

Poisson$set("public","pgf",function(z){
  exp(self$getParameterValue("rate")*(z-1))
})

Poisson$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1 = x1, lower.tail = FALSE, log.p = log.p)
})

Poisson$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

Poisson$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})




Poisson$set("public","initialize",function(rate=1, decorators = NULL, ...){
  
  private$.parameters <- ParameterSet$new(id = list("rate"), value = list(1),
                                          lower = list(0), upper = list(Inf),
                                          class = list("numeric"),
                                          settable = list(TRUE),
                                          updateFunc = list(NULL),
                                          description = list("Rate of events"))
  
  if(!is.null(rate)) self$setParameterValue(list(rate=rate))
  
  if(rate>=10)
    symmetric <- TRUE
  else
    symmetric <- FALSE
  
  
  pdf <- function(x1) dpois(x1, self$getParameterValue("rate"))
  cdf <- function(x1) ppois(x1, self$getParameterValue("rate"))
  quantile <- function(p) qpois(p, self$getParameterValue("rate"))
  rand <- function(n) rchisq(n, self$getParameterValue("rate"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosIntegers$new(zero = T), distrDomain = PosIntegers$new(zero = T),
                   symmetric = symmetric)
  
  

  invisible(self)
})
