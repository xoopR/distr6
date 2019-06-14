Weibull <- R6::R6Class("Weibull", inherit = Distribution, lock_objects = F)
Weibull$set("public","name","Weibull")
Weibull$set("public","short_name","Weibull")
Weibull$set("public","traits",list(type = PosReals$new(zero=T),
                                   valueSupport = "continuous",
                                   variateForm = "univariate"))
Weibull$set("public","description","Weibull Probability Distribution.")

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
  skew<-(digamma(1+3/k)*l^3-3*mu*sigma^2-mu^3)/sigma^3
  kur<-(l^4*digamma(1+4/k)-4*skew*sigma^3*mu-6*sigma^2*mu^2-mu^4)/sigma^4 - 3
  if(excess)
    return(kur)
  else
    return(kur+3)
})

Weibull$set("public","entropy",function(base = 2){
  -digamma(1)*(1-1/k)+log(l/k)+1
})



Weibull$set("public","mode",function(){
  if(k>1){
  return (l*((k-1)/k)^(1/k))
  }
  else{
  return(0)
  }
})

Weibull$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape=shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale=scale))
  return(lst)
})

Weibull$set("public","initialize",function(shape = 1, scale= 1, decorators = NULL,...){
  
  private$.parameters <- getParameterSet(self, shape, scale, verbose)
  self$setParameterValue(list(shape=shape,rate = rate ))
   
  pdf <- function(x1) dWeibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  cdf <- function(x1) pWeibull(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  quantile <- function(p) qWeibull(p, self$getParameterValue("shape"), self$getParameterValue("scale"))
  rand <- function(n) rWeibull(n, self$getParameterValue("shape"), self$getParameterValue("scale"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric = FALSE)
  
  invisible(self)
})
