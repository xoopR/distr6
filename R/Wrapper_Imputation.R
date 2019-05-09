Imputation <- R6::R6Class("Imputation", inherit = DistributionWrapper, lock_objects = FALSE)
Imputation$set("public","initialize",function(distribution, ...){
  distlist = list(distribution$clone())
  assertDistributionList(distlist)
  names(distlist) = distribution$short_name()

  super$initialize(distlist = distlist, type = distribution$type(),
                   support = distribution$support(),
                   distrDomain = distribution$distrDomain(), ...)
}) # IN PROGRESS

ImputeCDF <- R6::R6Class("ImputeCDF", inherit = Imputation)
ImputeCDF$set("public","initialize",function(distribution, strategy){
 if(strategy == "pdf2cdf"){
   if(testDiscrete(distribution))
     cdf = function(x) sum(self$pdf(self$sup():x))
   else if(testContinuous(distribution))
     cdf = function(x) integrate(self$pdf, lower = self$sup(), upper = x)
   formals(cdf) = list(self = distribution)
 }
  super$initialize(distribution = distribution, cdf = cdf, type = distribution$type(),
                   support = distribution$support(), distrDomain = distributon$distrDomain(),
                   symmetric = distribution$symmetric(), name =  name, short_name = short_name)
})

ImputePDF <- R6::R6Class("ImputePDF", inherit = Imputation)
ImputePDF$set("public","initialize",function(distribution, strategy){

})

ImputeQuantile <- R6::R6Class("ImputeQuantile", inherit = Imputation)
ImputeQuantile$set("public","initialize",function(distribution, strategy){

})

ImputeRand <- R6::R6Class("ImputeRand", inherit = Imputation)
ImputeRand$set("public","initialize",function(distribution, strategy){

})



Distribution$set("private",".genQ2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    return(sapply(1:n, function(x) self$quantile(runif(1))))
  }, list(aComment = "Sampling derived from quantile function"))
}) # NEEDS TESTING
Distribution$set("private",".genC2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    message("Results from numerical inversion may not be exact.")
    return(sapply(1:n, function(x) GoFKernel::inverse(private$.cdf)(runif(1))))
  }, list(aComment = "Sampling derived from cumulative distribution function via inverse transform sampling using `inverse` function from GoFKernel"))
}) # NEEDS TESTING
Distribution$set("private",".genP2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    if(testDiscrete(self))
      cdf = function(x) sum(self$pdf(self$inf():self$pdf(x)))
    else if(testContinuous(self)){
      message("Results from numerical integration are approximate only.")
      cdf = function(x) integrate(self$pdf, lower = self$inf(), upper = x)$value
    }
    message("Results from numerical inversion may not be exact.")
    return(sapply(1:n,function(x) GoFKernel::inverse(cdf)(runif(1))))
  }, list(aComment = "Sampling derived from numerical approximation of distribution function and inverse transform sampling using `inverse` function from GoFKernel"))
}) # NEEDS TESTING