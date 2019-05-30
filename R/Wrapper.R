#' @name DistributionWrapper
#' @title Abstract Wrapper for Distributions
#'
#' @details This wrapper is an abstract class and cannot be implemented directly.
#' See \code{\link{listWrappers}} for a list of wrappers that can be constructed. After wrapping multiple models, parameter IDs are altered by prefixing the ID with "model_".
#' For example wrapping Model1 with a parameter 'param1' results in 'Model1_param1'.
#' See \code{parameters} to find the parameter IDs.
#
#' @description An R6 abstract wrapper class with methods implemented for child classes.
#' @seealso \code{\link{TruncatedDistribution}}, \code{\link{HuberizedDistribution}}
#'
#'
#' @section Public Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input -> Output} \tab \strong{Details} \cr
#' \code{wrappedModels(model = NULL)} \tab character -> distribution \tab See public method details.\cr
#'}
#'
#' @section Public Method Details:
#' If a name is given to \code{wrappedModels}, returns the wrapped distribution otherwise returns a list
#' of all wrapped distributions.
#'
#'
NULL

#' @export
DistributionWrapper <- R6::R6Class("DistributionWrapper", inherit = Distribution, lock_objects = FALSE)
DistributionWrapper$set("public","initialize",function(distlist, prefixParams = TRUE,...){
  if(RSmisc::getR6Class(self) == "DistributionWrapper")
    stop(paste(RSmisc::getR6Class(self), "is an abstract class that can't be initialized."))

  assertDistributionList(distlist)

  lapply(distlist, function(x) x$parameters()$update())
  private$.wrappedModels <- distlist

  if(prefixParams){
    params <- do.call(rbind.data.frame,lapply(distlist, function(x){
      params = x[["parameters"]]()$as.data.frame()
      params[,1] = paste(x[["short_name"]],params[,1],sep="_")
      return(params)
    }))
    row.names(params) <- NULL
    params <- as.ParameterSet(params)
  } else{
    if(length(distlist) == 1)
      params <- distlist[[1]]$parameters()
    else
      params <- do.call(rbind,lapply(distlist, function(x) x$parameters()))
  }

  super$initialize(parameters = params, ...)
})

DistributionWrapper$set("private", ".wrappedModels", list())
DistributionWrapper$set("public", "wrappedModels", function(model=NULL){
  if(!is.null(model))
    return(private$.wrappedModels[[model]])
  else
    return(private$.wrappedModels)
})
DistributionWrapper$set("public","setParameterValue",function(lst){
  for(i in 1:length(lst)){
    if(grepl("_",lst[[i]],fixed = T)){
      id = names(lst)[[i]]
      underscore = gregexpr("_",id,fixed=T)[[1]][1]
      model = substr(id,1,underscore-1)
      parameter = substr(id,underscore+1,1000)

      value = lst[[i]]
      newlst = list(value)
      names(newlst) = parameter
    } else{
      model = self$wrappedModels()[[1]]$short_name
      newlst = lst
    }
    self$wrappedModels(model)$setParameterValue(newlst)
  }
  rm(i)

  params <- do.call(rbind,lapply(self$wrappedModels(), function(x){
    params = x[["parameters"]]()$as.data.frame()
    params[,1] = paste(x[["short_name"]],params[,1],sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  private$.parameters <- as.ParameterSet(params)

  unlockBinding("properties",self)
  self$properties$support <- do.call(product,lapply(self$wrappedModels(),function(x) x$support()))
  lockBinding("properties",self)

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
