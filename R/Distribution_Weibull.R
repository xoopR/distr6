library(R6)
library(distr6)
Weibull <- R6::R6Class("Weibull", inherit = Distribution, lock_objects = F)
Weibull$set("public","name","Weibull")
Weibull$set("public","short_name","Weibull")
Weibull$set("public","traits",list(type = PosReals$new(zero=T),
                                   valueSupport = "continuous",
                                   variateForm = "univariate"))


Weibull$set("public","mean",function(){
  self$getParameterValue("scale")*digamma(1+1/self$getParameterValue("shape"))
})

Weibull$set("public","var",function(){
  l<-self$getParameterValue("scale")
  k<-self$getParameterValue("shape")
  l^2 *(digamma(1+2/k)-digamma(1+1/k))^2
})


Weibull$set("public","skewness",function() {
  mu<-self$getParameterValue("mean")
  sigma<-sqrt(self$getParameterValue("var"))
  (digamma(1+3/k)*l^3-3*mu*sigma^2-mu^3)/sigma^3
})

Weibull$set("public","kurtosis",function(excess = TRUE){
  kur<-(l^4*digamma(1+4/k)-4*self$getParameterValue("skewness")*sigma^3*mu-6*sigma^2*mu^2-mu^4)/sigma^4 - 3
  if(excess)
    return(kur)
  else
    return(kur+3)
})

Weibull$set("public","entropy",function(base = 2){
  -digamma(1)*(1-1/k)+log(l/k)+1
})


Weibull$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1, lower.tail = FALSE, log.p)
})

Weibull$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

Weibull$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})

Weibull$set("public","mode",function(){
  return(max(self$getParameterValue("df")-2,0))
})

Weibull$set("private",".parameters", NULL)

Weibull$set("public","initialize",function(shape = 1, scale= 1, decorators = NULL,...){
  
  
  private$.parameters <- ParameterSet$new(id = list("shape","scale"),
                                          value = list(1, 1),
                                          lower = list(0, 0 ),
                                          upper = list(Inf, Inf),
                                          class = list("numeric","numeric"),
                                          settable = list(TRUE, TRUE),
                                          updateFunc = list(NA, NA),
                                          description = list("Shape",
                                                             "Scale"))
  
  
  self$setParameterValue(list(shape=shape,scale=scale))
  
  
  pdf <- function(x1) dWeibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  cdf <- function(x1) pWeibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  quantile <- function(p) qWeibull(p, self$getParameterValue("shape"), self$getParameterValue("scale"))
  rand <- function(n) rWeibull(n, self$getParameterValue("shape"), self$getParameterValue("scale"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric = FALSE)
  
  invisible(self)
})
