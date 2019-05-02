HuberizedDistribution <- R6::R6Class("HuberizedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
HuberizedDistribution$set("private", ".cutoffInterval", NULL)
HuberizedDistribution$set("public", "getLowerLimit", function(){
  return(private$.cutoffInterval[[1]])
})
HuberizedDistribution$set("public", "getUpperLimit", function(){
  return(private$.cutoffInterval[[2]])
})
HuberizedDistribution$set("public","initialize",function(distribution, lower, upper){

  assertDistribution(distribution)

  if(missing(lower)) lower = distribution$inf()
  if(missing(upper)) upper = distribution$sup()

  pdf <- function(x, ...){
    if(x <= self$getLowerLimit())
      return(self$wrappedModels()[[1]]$cdf(self$getLowerLimit()))
    else if(x >= self$getUpperLimit())
      return(1-self$wrappedModels()[[1]]$cdf(self$getUpperLimit()))
    else
      return(self$wrappedModels()[[1]]$pdf(x))
  }

  name = paste("Huberized",distribution$name())
  short_name = paste0("Huberized",distribution$short_name())

  distlist = list(distribution)
  names(distlist) = distribution$short_name()

  private$.cutoffInterval = c(lower, upper)

  super$initialize(distlist = distlist, pdf = pdf, name = name,
                   short_name = short_name, type = reals$new())
}) # IN PROGRESS
