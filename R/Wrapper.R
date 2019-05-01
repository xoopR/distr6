DistributionWrapper <- R6::R6Class("DistributionWrapper", inherit = Distribution)
DistributionWrapper$set("public","initialize",function(distlist, ...){
  if(getR6Class(self) == "DistributionWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertDistributionList(distlist)

  private$.wrappedModels <- distlist

  params <- do.call(rbind,lapply(distlist, function(x){
    params = x[["parameters"]]()
    params[,1] = paste(x[["short_name"]](),params[,1],sep="_")
    return(params)
    }))
  row.names(params) <- NULL

  super$initialize(parameters = params,...)
})

DistributionWrapper$set("private", ".wrappedModels", list())
DistributionWrapper$set("public", "wrappedModels", function(model=NULL){
  if(!is.null(model))
    return(private$.wrappedModels[[model]])
  else
    return(private$.wrappedModels)
})
DistributionWrapper$set("public", "getInternalModel", function(model){
  return(private$.wrappedModels[[model]])
})

DistributionWrapper$set("public","setParameterValue",function(lst){
  for(i in 1:length(lst)){
    id = names(lst)[[i]]
    underscore = gregexpr("_",id,fixed=T)[[1]][1]
    model = substr(id,1,underscore-1)
    parameter = substr(id,underscore+1,1000)

    value = lst[[i]]
    newlst = list(value)
    names(newlst) = parameter

    self$getInternalModel(model)$setParameterValue(newlst)
  }

  params <- do.call(rbind,lapply(private$.wrappedModels, function(x){
    params = x[["parameters"]]()
    params[,1] = paste(x[["short_name"]](),params[,1],sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  private$.parameters <- params

  invisible(self)
}) # NEEDS TESTING