
Poisson <- R6::R6Class("Poisson", inherit = SDistribution, lock_objects = F)
Poisson$set("public","name","Poisson")
Poisson$set("public","short_name","Pois")
Poisson$set("public","traits",list(type = PosIntegers$new(zero = T),
                                   valueSupport = "discrete",
                                   variateForm = "univariate"))

Poisson$set("public","description","Poisson Probability Distribution.")

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


Poisson$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  return(lst)
})





Poisson$set("public","initialize",function(rate=1, decorators = NULL, ...){
  
  
  private$.parameters <- getParameterSet(self, rate, verbose)
  self$setParameterValue(list(rate = rate))
  
  if(rate>=30)
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

Poisson$set("public", "cf", function(t){
  exp(self$getParameterValue("rate")*(exp(1i*t)-1))
})

Poisson$set("public","pgf",function(z){
  exp(self$getParameterValue("rate")*(z-1))
})


Poisson$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  return(lst)
})





Poisson$set("public","initialize",function(rate=1, decorators = NULL, ...){
  
  
  private$.parameters <- getParameterSet(self, rate, verbose)
  self$setParameterValue(list(rate = rate))
  
  if(rate>=30)
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
