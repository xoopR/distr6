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