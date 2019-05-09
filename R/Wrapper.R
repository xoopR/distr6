#' @name DistributionWrapper
#' @title Abstract Wrapper for Distributions
#' @description An R6 abstract wrapper class with methods implemented for child classes.
#' @seealso \code{\link{TruncatedDistribution}}, \code{\link{HuberizedDistribution}}
#' @details Cannot be implemented directly.
#' @section Public Methods:
#' \tabular{ll}{
#' \code{getWrappedModels(model = NULL)} \tab Get internally wrapped model by name. Or list of all models if model = NULL. \cr
#' \code{setParameterValue(lst)} \tab Sets the value of an internal models parameter. See Details.
#' }
#' @section Public Method Arguments:
#' \tabular{ll}{
#' \code{model} \tab Wrapped model to access. \cr
#' \code{lst} \tab list. Names are parameter IDs, values are values to set parameters.
#' }
#'
#' @section Public Methods Details:
#' Wrapped models overload the minimum number of functions necessary. The abstract class overloads
#' the \code{setParameterValue} method only as wrapped models alter paramater IDs. After wrapping
#' a model, parameter IDs are altered by prefixing the ID with "model_". For example wrapping Model1 with
#' a parameter 'param1' results in 'Model1_param1'.
#'
NULL

#' @export
DistributionWrapper <- R6::R6Class("DistributionWrapper", inherit = Distribution, lock_objects = FALSE)
DistributionWrapper$set("public","initialize",function(distlist, ...){
  if(getR6Class(self) == "DistributionWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertDistributionList(distlist)

  lapply(distlist, function(x) x$parameters()$update())
  private$.wrappedModels <- distlist

  params <- do.call(rbind,lapply(distlist, function(x){
    params = x[["parameters"]](as.df = T)
    params[,1] = paste(x[["short_name"]](),params[,1],sep="_")
    return(params)
    }))
  row.names(params) <- NULL
  params <- as.ParameterSet(params)

  super$initialize(parameters = params, ...)
})

DistributionWrapper$set("private", ".wrappedModels", list())
DistributionWrapper$set("public", "getWrappedModels", function(model=NULL){
  if(!is.null(model))
    return(private$.wrappedModels[[model]])
  else
    return(private$.wrappedModels)
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
    self$getWrappedModels(model)$setParameterValue(newlst)
  }

  params <- do.call(rbind,lapply(private$.wrappedModels, function(x){
    params = x[["parameters"]](as.df = T)
    params[,1] = paste(x[["short_name"]](),params[,1],sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  private$.parameters <- as.ParameterSet(params)

  invisible(self)
}) # NEEDS TESTING

#' @name ConcreteWrapper
#' @title Concrete Wrapper for Distributions
#' @description An R6 concrete wrapper class for use with decorators.
#' @seealso \code{\link{DistributionWrapper}}
#' @details Should not be constructed by the user. The purpose of this class is to provide a non-abstract
#' interface that can be initialized by decorators when required.
#'
NULL

#' @export
ConcreteWrapper <- R6::R6Class("ConcreteWrapper", inherit = DistributionWrapper, lock_objects = FALSE)
ConcreteWrapper$set("public","initialize",function(...){
  super$initialize(...)
})