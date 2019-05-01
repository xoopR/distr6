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

Convolution <- R6::R6Class("Convolution", inherit = DistributionWrapper)
Convolution$set("public","initialize",function(dist1, dist2, add = TRUE,...){
  distlist = list(dist1$clone(), dist2$clone())
  assertDistributionList(distlist)
  names(distlist) = c(dist1$short_name(), dist2$short_name())


  if(testContinuous(dist1) & testContinuous(dist2)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
        warning("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(z - y)*
              self$getInternalModel(name2)$pdf(y)},
                    lower = self$getInternalModel(name2)$inf(), upper = z)$value
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    } else {
      body(fnc) <- substitute({
        warning("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(y - z)*
              self$getInternalModel(name2)$pdf(y)},
                    lower = self$getInternalModel(name2)$inf(), upper = z)$value
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    }
  } else if(testDiscrete(dist1) & testDiscrete(dist2)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(self$getInternalModel(name2)$inf(), z, by = 1)
          sum(self$getInternalModel(name1)$pdf(z - support) *
                self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    } else {
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(self$getInternalModel(name2)$inf(), z, by = 1)
          sum(self$getInternalModel(name1)$pdf(support - z) * self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    }
  }

  name = paste("Convolution of",dist1$short_name(),"and",dist2$short_name())
  short_name = paste0(dist1$short_name(),dist2$short_name())

  super$initialize(distlist = distlist, pdf = fnc, name = name,
                   short_name = short_name, ...)
}) # IN PROGRESS
