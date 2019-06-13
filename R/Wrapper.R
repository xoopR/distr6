#' @name DistributionWrapper
#' @title Abstract Wrapper for Distributions
#' @description An R6 abstract wrapper class with methods implemented for child classes.
#'
#' @details This wrapper is an abstract class and cannot be implemented directly.
#' See \code{\link{listWrappers}} for a list of wrappers that can be constructed. After wrapping multiple models,
#' parameter IDs are altered by prefixing the ID with "model_". For example wrapping Model1 with a parameter
#' 'param1' results in 'Model1_param1'. Call \code{parameters} to find the parameter IDs.
#'
#' @seealso \code{\link{listWrappers}}
#'
#'
#' @section Public Methods:
#' \tabular{ll}{
#' \strong{Method} \tab \strong{Link} \cr
#' \code{wrappedModels(model = NULL)} \tab \code{\link{wrappedModels}} \cr
#'}
#'
#'
NULL
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

#' @name wrappedModels
#' @title Gets Internally Wrapped Models
#' @description Returns either a list of all the wrapped models or the models named by parameters.
#'
#' @usage wrappedModels(object, model = NULL)
#' @section R6 Usage: $wrappedModels(model = NULL)
#'
#' @param object Distribution.
#' @param model character, see details.
#'
#' @details Accessor for internally wrapped models. If the \code{model} parameter is matched by a single named
#' wrapped model, this model is returned. If a vector is supplied to \code{model} parameter then a list
#' of internal models is returned if matched, otherwise a list of all internal models is returned. If
#' \code{model} is NULL (default) then a list of all internal models are returned.
#'
#' @seealso \code{\link{DistributionWrapper}}
#'
#' @export
NULL
DistributionWrapper$set("public", "wrappedModels", function(model=NULL){

  if(!is.null(model)){
    if(all(model %in% names(private$.wrappedModels))){
      if(length(model)==1)
        return(private$.wrappedModels[[model]])
      else
        return(private$.wrappedModels[model])
    } else
      private$.wrappedModels
  } else
    private$.wrappedModels
})
DistributionWrapper$set("private", ".wrappedModels", list())
DistributionWrapper$set("public","setParameterValue",function(lst, error = "warn"){
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
    self$wrappedModels(model)$setParameterValue(newlst, error)
  }
  rm(i)

  params <- do.call(rbind,lapply(self$wrappedModels(), function(x){
    params = x[["parameters"]]()$as.data.frame()
    params[,1] = paste(x[["short_name"]],params[,1],sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  private$.parameters <- as.ParameterSet(params)

  private$.properties$support <- do.call(product,lapply(self$wrappedModels(),function(x) x$support()))

  invisible(self)
})
