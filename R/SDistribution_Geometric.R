# remotes::install_github("RaphaelS1/distr6", ref = "dev",force=TRUE)
library(dplyr)
library(distr6)
library(R6)

Geometric <-  R6::R6Class("Geometric", inherit = SDistribution, lock_objects = F)
Geometric$set("public","name","Geometric")
Geometric$set("public","short_name","Geom")

Geometric$set("public","traits",list(type = PosIntegers$new(zero = T),
                                       valueSupport = "discrete",
                                       variateForm = "univariate"))

Geometric$set("public","description","Geometric Distribution.")

Geometric$set("private",".pdf",function(x1){
   return(dgeom(x1,self$getParameterValue("prob")))
})

Geometric$set("private",".cdf",function(x1){
   return(pgeom(x1,self$getParameterValue("prob")))
})

Geometric$set("private",".quantile",function(p){
    return(qgeom(p,self$getParameterValue("prob")))
})

Geometric$set("private",".rand",function(n){
    return(rgeom(n,self$getParameterValue("prob")))
})

Geometric$set("public","expectation",function(){
   return(1/self$getParameterValue("prob"))
})

Geometric$set("public","var",function(){
   return((1-self$getParameterValue("prob"))/(self$getParameterValue("prob")^2))
})

Geometric$set("public","skewness",function(){
    return((2-self$getParameterValue("prob"))/sqrt(1-self$getParameterValue("prob")))
})

Geometric$set("public","kurtosis",function(excess = TRUE){
    exkurtosis = 5-self$getParameterValue("prob")+1/(1-self$getParameterValue("prob"))
    if(excess)
        return(exkurtosis)
    else
        return(exkurtosis + 3)
})

Geometric$set("public","entropy",function(base = 2){
   return((-(1-self$getParameterValue("prob"))*log(1-self$getParameterValue("prob"),base)-self$getParameterValue("prob")*log(self$getParameterValue("prob"),base))/self$getParameterValue("prob"))
})

Geometric$set("public", "mgf", function(t){
   return((self$getParameterValue("prob")*exp(t))/(1-(1-self$getParameterValue("prob"))*exp(t)))
})

Geometric$set("public", "cf", function(t){
    return((self$getParameterValue("prob")*exp(1i*t))/(1-(1-self$getParameterValue("prob"))*exp(1i*t)))
})

Geometric$set("public","pgf",function(z){
   return((1-self$getParameterValue("prob"))/(1-z*self$getParameterValue("prob")))
})

Geometric$set("public","survival",function(x1, log.p = FALSE){
    return((1-self$getParameterValue("prob"))^x1)
})

Geometric$set("public","hazard",function(x1){
    return(self$pdf(x1)/self$survival(x1))
})

Geometric$set("public","cumHazard",function(x1){
    return(-self$cdf(x1, log.p = TRUE))
})

Geometric$set("public","mode",function() return(0))

Geometric$set("public","setParameterValue",function(lst){
    super$setParameterValue(lst)
    unlockBinding("properties", self)
    self$properties$support <- Set$new(0:self$getParameterValue("prob"))
    lockBinding("properties", self)
})

Geometric$set("private",".parameters", NULL)

Geometric$set("private",".getRefParams", function(paramlst){
    lst = list()
    if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
    return(lst)
})

Geometric$set("public","initialize",function(prob = 0.5, decorators = NULL,...){
    
    private$.parameters <- getParameterSet(self, prob)
    self$setParameterValue(list(prob = prob))
    
    pdf <- function(x1) dgeom(x1, self$getParameterValue("prob"))
    cdf <- function(x1) pgeom(x1, self$getParameterValue("prob"))
    quantile <- function(p) qgeom(p, self$getParameterValue("prob"))
    rand <- function(n) rgeom(n, self$getParameterValue("prob"))
    
    super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                     rand = rand, support = PosIntegers$new(zero = T), distrDomain = PosIntegers$new(zero = T),
                     symmetric  = FALSE)
    
    invisible(self)
    
})


