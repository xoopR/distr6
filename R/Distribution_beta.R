library(distr6)
library("R6")

Beta <- R6::R6Class("Beta", inherit = Distribution, lock_objects = F)
Beta$set("public","name","Beta")
Beta$set("public", "triats",list(type = PosReals$new(),
                                 valueSupport ="continuous",
                                 variateForm = "univariate"))
Beta$set("public","description","Beta Probability Distribution.")


Beta$set("public","mean",function(){
  self$getParameterValue("shape1") / (self$getParameterValue("shape1")+self$getParameterValue("shape2"))
})
Beta$set("public","var",function(){
  self$getParameterValue("shape1")*self$getParameterValue("shape2")*
    (self$getParameterValue("shape1")+self$getParameterValue("shape2")^-2)*
    (self$getParameterValue("shape1")+self$getParameterValue("shape2")+1)^-1
})
Beta$set("public","skewness",function(){
  2*(self$getParameterValue("shape2")-self$getParameterValue("shape1"))*
    ((self$getParameterValue("shape1")+self$getParameterValue("shape2")+1)^0.5)*
    ((self$getParameterValue("shape1")+self$getParameterValue("shape2")+2)^-1)*
    (self$getParameterValue("shape1")*self$getParameterValue("shape2"))^-0.5
})
Beta$set("public","kurtosis",function(excess = TRUE){
  
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  ex_kurtosis = 6*{((shape1-shape2)^2)*(shape1+shape2+1)-shape1*shape2*(shape1+shape2+2)}/
                (shape1*shape2*(shape1+shape2+2)*(shape1+shape2+3))
  if (excess)
    return(ex_kurtosis)
  else
    return(ex_kurtosis+3)
})
  
#Entropy
Beta$set("public", "entropy", function(base = 2){
  shape1 <- self$getParameterValue("shape1")
  shape2 <- self$getParameterValue("shape2")
  entropy = log(beta(shape1,shape2))-(shape1-1)*digamma(shape1)-(shape2-1)*digamma(shape2)+(shape1+shape2)*digamma((shape1+shape2))
  return(entropy)
})

Beta$set("public", "mgf", function(){
  message("No analytic result for beta mgf available. Try decorating with CoreStatistics.")
  return(NULL)
})

Beta$set("public", "cf", function(){
  message("No analytic result for beta  available. Try decorating with CoreStatistics.")
  return(NULL)
})

Beta$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape1)) lst = c(lst,list(shape1 = paramlst$shape1))
  if(!is.null(paramlst$shape2)) lst = c(lst,list(shape2 = paramlst$shape2))
  return(lst)
})

Beta$set("public", "initialize", function(shape1 = NULL, shape2 = NULL, decorators = NULL,
                                          verbose = FALSE){
  shape1.bool = FALSE
  shape2.bool = FALSE
  
  if(is.null(shape1) & is.null(shape2)){
    message("both shape parameters are missing. shape1 = shape2 =1 parameterisation used.")
    shape1 = shape2 = 1
  } 
  
  else if (is.null(shape1) & !is.null(shape2)){
    message("shape1 parameter is missing and shape2 parameterisation is used")
    shape2.bool = TRUE
    shape1 = 1
    shape2 = shape2
  }
  
  else if (is.null(shape2) & !is.null(shape1)){
    message("shape2 parameter is missing and shape1 parameterisation is used")
    shape1.bool = TRUE
    shape2 = 1
    shape1 = shape1
  }
  
  else {
    message("shape parameters are provided")
    shape1.bool = shape2.bool = TRUE
    shape1 = shape1 
    shape2 = shape2
  }
  
  
  private$.parameters <- ParameterSet$new(id = list("shape1","shape2"), value = list(1,1),
                                          lower = list(0,0), upper = list(Inf,Inf),
                                          class = list("integer","integer"),
                                          settable = list(shape1.bool,shape2.bool),
                                          description = list("shape1","shape2"))
  
  pdf <- function(x1) dbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  cdf <- function(x1) pbeta(x1, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  quantile <- function(p) qbeta(p, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  rand <- function(n) rbeta(n, self$getParameterValue("shape1"), self$getParameterValue("shape2"))
  
  
  if (shape1 == shape2)
    symmetric <- TRUE
  else
    symmetric <- FALSE
  
  super$initialize(decorators = decorators, pdf =pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(0,1), distrDomain = PosReals$new(zero = TRUE),
                   symmetric = symmetric)
  
  invisible(self)
})
