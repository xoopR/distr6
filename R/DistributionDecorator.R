DistributionDecorator <- R6::R6Class("DistributionDecorator")
DistributionDecorator$set("public","initialize",function(distribution){
  if(getR6Class(self) == "DistributionDecorator")
    stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Try using
               decorate([distribution], ",getR6Class(self),")"))

  decorate(distribution, get(getR6Class(self)))
})
