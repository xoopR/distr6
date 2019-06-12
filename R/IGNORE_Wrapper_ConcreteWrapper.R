# For internal use only
ConcreteWrapper <- R6::R6Class("ConcreteWrapper", inherit = DistributionWrapper, lock_objects = FALSE)
ConcreteWrapper$set("public","initialize",function(...){
  super$initialize(...)
})
