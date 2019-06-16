


library(distr6)
#Gamma distribution


Gamma <- R6::R6Class("Gamma", inherit = SDistribution, lock_objects = F)
Gamma$set("public","name","Gamma")
Gamma$set("public","short_name","Gam")

Gamma$set("public","traits",list(type = PosReals$new(zero = T),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))

Gamma$set("public","description","Gamma Probability Distribution.")

Gamma$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$rate)) lst = c(lst, list(rate = paramlst$rate))
  if(!is.null(paramlst$mean)) lst = c(lst, list(rate = paramlst$shape/paramlst$mean))
  if(!is.null(paramlst$scale)) lst = c(lst, list(rate = paramlst$scale^-1))
  return(lst)
})

Gamma$set("public","initialize",function(shape= 1,rate=1,mean=NULL, scale =NULL, decorators = NULL, verbose = FALSE){
  
  private$.parameters <- getParameterSet.Gamma(self, shape, scale, mean, rate, verbose)
  self$setParameterValue(list(shape=shape,rate = rate,mean=mean,scale = scale))
  
  pdf <- function(x1) dgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  cdf <- function(x1) pgamma(x1, self$getParameterValue("shape"),self$getParameterValue('rate'))
  quantile <- function(p) qgamma(p, self$getParameterValue("shape"),self$getParameterValue('rate'))
  rand <- function(n) rgamma(n, self$getParameterValue("shape"),self$getParameterValue('rate'))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = F), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE)
  invisible(self)
})

Gamma$set("public","mean",function(){
  self$getParameterValue("mean")
})

Gamma$set("public","var",function(){
  self$getParameterValue("mean")*self$getParameterValue("scale")
})

Gamma$set("public","skewness",function() {
  2/sqrt(self$getParameterValue("shape"))
})

  

Gamma$set("public","kurtosis",function(excess = TRUE){
  if(excess){
    6/self$getParameterValue("shape")
  }else{
    (6/self$getParameterValue("shape"))+3
  }
})

Gamma$set("public","entropy",function(base = 2){
  self$getParameterValue("shape") + log(self$getParameterValue("scale"), base)+log(gamma(self$getParameterValue("shape")), base)+ (1-self$getParameterValue("shape"))*digamma(self$getParameterValue("shape"))
})


Gamma$set("public", "mgf", function(t){
  if(t < 1/self$getParameterValue("scale"))
    return(   (1-self$getParameterValue("scale")*t)^(-self$getParameterValue("shape")) )
  else
    return(0)
})

Gamma$set("public", "cf", function(t){
  return(  (1-self$getParameterValue("scale")*1i*t )^(-self$getParameterValue("shape"))   )
})

Gamma$set("public","mode",function(){
  if(self$getParameterValue("shape")>=1){
    
    (self$getParameterValue("shape")-1)*self$getParameterValue("scale")
    
  }else{
    return(0)
  }

})










