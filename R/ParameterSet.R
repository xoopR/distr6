#' @title Make an R6 Parameter Set for Distributions
#'
#' @name ParameterSet
#' @description ParameterSets are passed to a \code{Distribution$new} constructor when
#'  creating a custom probability distribution that takes parameters.
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{id} \tab character \tab unique one-word identifier. \cr
#' \code{value} \tab numeric \tab initial parameter value. \cr
#' \code{support} \tab numeric \tab range of values parameter can take. \cr
#' \code{settable} \tab logical \tab if TRUE the parameter is printed. See Details. \cr
#' \code{updateFunc = NULL} \tab function \tab evaluated to update parameter. See Details. \cr
#' \code{description = NULL} \tab character \tab optional description of parameter.
#' }
#'
#' @section Constructor Details:
#' An R6 ParameterSet is required to construct a custom Probability Distribution that takes parameters.
#' This constructor ensures that the correct format of parameters is supplied to the distribution.
#'
#' Every argument can either be given as the type listed above or as a list of that type.
#' If arguments are provided as a list, then each argument must be of the same length list, with values
#' as NULL where appropriate. See examples for more.
#'
#' Each parameter requires a unique one-word \code{id} that is used to get and set parameters
#' after construction. The parameterisation of the distribution is determined by the parameters
#' that have \code{settable = TRUE}, this is a slightly confusing term as it actually refers to a parameter
#' being 'machine-settable'. Here it just means that the given parameter is used in construction and therefore
#' will be included in a call to \code{$print}. \code{updateFunc} is used to update the parameters not used in
#' the parameterisation. These should be given as a function that could be understood in the body of a Distribution
#' and should start with \code{function(self)}, see examples.
#'
#' Internally after calling \code{$setParameterValue}, \code{$update} is called to update all parameters
#' with a non-NA \code{updateFunc}.
#'
#'@section Public Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{print(hide_cols = c("updateFunc","settable"))} \tab \code{\link{print.ParameterSet}} \cr
#'   \code{update()} \tab \code{\link{update.ParameterSet}} \cr
#'   \code{parameters(id, error = "warn")} \tab \code{\link{parameters}} \cr
#'   \code{getParameterSupport(id, error = "warn")} \tab \code{\link{getParameterSupport}} \cr
#'   \code{getParameterValue(id, error = "warn")} \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(..., lst = NULL, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'   \code{merge(y, ...)} \tab \code{\link{merge.ParameterSet}} \cr
#'   \code{as.data.table()} \tab \code{\link{as.data.table}} \cr
#' }
#'
#' @examples
#'  id = list("prob", "size")
#'  value = list(0.2, 5)
#'  support = list(Interval$new(0,1), PosNaturals$new())
#'  settable = list(TRUE, TRUE)
#'  description = list("Probability of success",NULL)
#'  ps = ParameterSet$new(id, value, support, settable,
#'                        description = description)
#'  ps$parameters()
#'  ps$getParameterValue("prob")
#'  ps$getParameterSupport("prob")
#'
#'
#' @examples
#'  id = list("rate", "scale")
#'  value = list(1, 1)
#'  support = list(PosReals$new(), PosReals$new())
#'  settable = list(TRUE, FALSE)
#'  updateFunc = list(NULL, function(self) 1/self$getParameterValue('rate'))
#'  description = list("Arrival rate","Scale parameter")
#'  ps = ParameterSet$new(id, value, support, settable,
#'                        updateFunc, description)
#'  ps$parameters(id = "rate")
#'  ps$setParameterValue(rate = 2) # Automatically calls $update
#'  ps$getParameterValue("scale") # Auto-updated to 1/2
#'
#' @seealso \code{\link{Distribution}}
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
NULL

ParameterSet <- R6::R6Class("ParameterSet")
ParameterSet$set("private",".parameters",NULL)
ParameterSet$set("private",".SetParameterSupport",function(lst){
  id = names(lst)
  support = lst[[1]]
  which = which(unlist(private$.parameters$id) %in% id)
  private$.parameters[which,3][[1]] <- list(support)
  invisible(self)
})
ParameterSet$set("public","initialize", function(id, value, support, settable,
                                                 updateFunc = NULL, description = NULL){

  checkmate::assert(length(id)==length(value), length(id)==length(settable),
                    length(id)==length(support), combine = "and",
                    .var.name = "all arguments must be of same length")
  if(!is.null(description))
    checkmate::assert(length(id)==length(description), .var.name = "all arguments must be of same length")
  if(!is.null(updateFunc))
    checkmate::assert(length(id)==length(updateFunc), .var.name = "all arguments must be of same length")
  checkmate::assert(!any(duplicated(id)), .var.name = "'id's must be unique")

  params = data.table::data.table()
  for(i in 1:length(id)){
    a_id = id[[i]]
    checkmate::assertCharacter(a_id,.var.name = "'id' must be character")
    checkmate::assert(length(strsplit(a_id," ",fixed=T)[[1]])==1,
                      .var.name = "'id' must be one word")

    a_settable  = settable[[i]]
    checkmate::assertLogical(a_settable, .var.name = "'settable' must be logical")

    a_support = support[[i]]
    checkmate::assert(inherits(a_support, "SetInterval"),
                      .var.name = "'class' must inherit class 'SetInterval'")

    if(!is.null(description)){
      a_description = description[[i]]
      if(is.null(a_description)) a_description = "None"
      checkmate::assertCharacter(a_description,.var.name = "'description' must be character")
    } else
      a_description = "None"

    if(!is.null(updateFunc)){
      a_update = updateFunc[[i]]
      if(is.null(a_update))
        a_update = NA
    } else
      a_update = NA

    a_value = as(value[[i]], a_support$.__enclos_env__$private$.class)
    checkmate::assert(a_support$liesInSetInterval(a_value, all = T), combine = "and",
                      .var.name = "'value' should be between 'lower' and 'upper'")

    a_param = data.table::data.table(id = a_id, value = a_support$min(), support = list(a_support),
                         settable = a_settable,
                         description = a_description,
                         updateFunc = a_update,
                         stringsAsFactors = F)
    a_param$value <- list(a_value)

    params = rbind(params, a_param)
  }

  private$.parameters <- params
  invisible(self)
})
#' @name print.ParameterSet
#' @title Print a ParameterSet
#'
#' @description Prints a ParameterSet as a data.table with strprint variants of R6 classes.
#' @details If given the \code{hide_cols} argument can be used to hide specific columns from the
#' data.table.
#'
#' @param x ParameterSet
#' @param hide_cols string, if given the data.table is filtered to hide these columns
#' @param ... ignored, added for S3 consistency
#'
#' @section R6 Usage: $print(hide_cols = c("updateFunc","settable"))
#'
#' @seealso \code{\link{ParameterSet}}
#'
#' @export
print.ParameterSet <- function(x, hide_cols, ...) {}
ParameterSet$set("public","print", function(hide_cols = c("updateFunc","settable"),...){
  ps <- private$.parameters
  ps$support <- lapply(ps$support,function(x) x$getSymbol())
  print(subset(ps, select = !(names(ps) %in% hide_cols)))
})

#' @title Updates a ParameterSet
#'
#' @importFrom stats update
#' @section R6 Usage: $update()
#'
#' @param object ParameterSet
#' @param ... ignored, added for S3 consistency
#'
#' @description Updates parameter in a ParameterSet using \code{updateFunc}s.
#'
#' @details In general this method should never need to be called manually by the user as it is internally
#' called in \code{setParameterValue}.
#'
#' The method works by cycling through parameters in a \code{ParameterSet} that have non-NA \code{updateFunc}s
#' and parses these as expressions, thereby updating their values.
#'
#' @seealso \code{\link{ParameterSet}}
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
update.ParameterSet <- function(object, ...) {}
ParameterSet$set("public","update", function(...){
  if(any(!is.na(private$.parameters$updateFunc))){
    update_filter = !is.na(private$.parameters$updateFunc)
    updates = private$.parameters[update_filter,]
    newvals = apply(updates, 1, function(x) return(x[[6]](self)))
    suppressWarnings(data.table::set(private$.parameters, which(update_filter), "value", as.list(newvals)))
  }

  invisible(self)
})

#' @name parameters
#' @title Parameters Accessor
#' @description Returns some or all the parameters in a distribution.
#'
#' @usage parameters(object, id = NULL)
#' @section R6 Usage: $parameters(id = NULL)
#'
#' @param object Distribution or ParameterSet.
#' @param id character, see details.
#'
#' @details If \code{id} is given and matches a parameter in the distribution, the parameter is returned
#' with all details. If \code{id} is given but doesn't match a parameter, an empty data.table is returned.
#' Finally if \code{id} is not given, returns self.
#'
#' @seealso \code{\link{getParameterValue}} and \code{\link{setParameterValue}}
#'
#' @return An R6 object of class ParameterSet or a data.table.
#'
#' @export
NULL
ParameterSet$set("public","parameters",function(id = NULL){
  if(!is.null(id)){
    id0 = id
    if(nrow(subset(private$.parameters, id %in% id0))==0)
      return(self)
    else
      return(subset(private$.parameters, id %in% id0))
  } else {
      return(self)
  }
})

#' @name getParameterSupport
#' @title Parameter Support Accessor
#' @description Returns the support of the given parameter.
#' @usage getParameterSupport(object, id, error = "warn")
#' @section R6 Usage: $getParameterSupport(id, error = "warn")
#' @param object Distribution or ParameterSet.
#' @param id character, id of the parameter to return.
#' @param error character, value to pass to \code{stopwarn}.
#' @details Returns NULL and warning if the given parameter is not in the Distribution, otherwise returns
#' the support of the given parameter as a SetInterval object.
#'
#' \code{stopwarn} either breaks the code with an error if "error" is given or returns \code{NULL}
#' with warning otherwise.
#'
#' @return An R6 object of class SetInterval.
#'
#' @seealso \code{\link{parameters}} and \code{\link{SetInterval}}
#' @export
NULL
ParameterSet$set("public","getParameterSupport",function(id, error = "warn"){
  if(missing(id))
    return(stopwarn(error, "Argument 'id' is missing, with no default."))

  support = self$parameters(id)[["support"]]
  if(length(support)==0){
    return(stopwarn(error, paste(id, "is not a parameter in this distribution.")))
  }else
    return(unlist(support[[1]]))

})

#' @name getParameterValue
#' @title Parameter Value Accessor
#' @description Returns the value of the given parameter.
#' @usage getParameterValue(object, id, error = "warn")
#' @section R6 Usage: $getParameterValue(id, error = "warn")
#' @param object Distribution or ParameterSet.
#' @param id character, id of the parameter to return.
#' @param error character, value to pass to \code{stopwarn}.
#' @details Returns NULL and warning if the given parameter is not in the Distribution, otherwise returns
#' the value of the given parameter.
#'
#' \code{stopwarn} either breaks the code with an error if "error" is given or returns \code{NULL}
#' with warning otherwise.
#'
#' @return The current value of a given parameter as a numeric.
#'
#' @seealso \code{\link{parameters}} and \code{\link{setParameterValue}}
#' @export
NULL
ParameterSet$set("public","getParameterValue",function(id, error = "warn"){
  if(missing(id))
    return(stopwarn(error, "Argument 'id' is missing, with no default."))
  val = self$parameters(id)[["value"]]
  if(length(val)==0){
    return(stopwarn(error, paste(id, "is not a parameter in this distribution.")))
  }else
    return(unlist(val[[1]]))

})

#' @name setParameterValue
#' @title Parameter Value Setter
#' @description Returns the value of the given parameter.
#'
#' @usage setParameterValue(object, ..., lst = NULL, error = "warn")
#' @section R6 Usage: $setParameterValue(..., lst = NULL, error = "warn")
#' @param object Distribution or ParameterSet.
#' @param ... named parameters and values to update, see details.
#' @param lst optional list, see details.
#' @param error character, value to pass to \code{stopwarn}.
#' @details Parameters can be updated in one of two ways, either by passing the parameters to update
#' as named arguments or as a list with the the list names are parameter IDs and the list values are
#' the respective values to set the parameters. Using a list may be preferred for parameters that take
#' multiple values. See examples. If \code{lst} is given then any additional arguments are ignored.
#'
#' \code{stopwarn} either breaks the code with an error if "error" is given or returns \code{NULL}
#' with warning otherwise.
#'
#' @seealso \code{\link{parameters}} and \code{\link{setParameterValue}}
#'
#' @return An R6 object of class ParameterSet.
#'
#' @examples
#' ps <- Normal$new()$parameters()
#' ps$setParameterValue(mean  = 2, var = 5)$print()
#'
#' ps <- MultivariateNormal$new()$parameters()
#' ps$setParameterValue(lst = list(mean = c(1,1)))$print()
#'
#' @export
NULL
ParameterSet$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
    if(is.null(lst))
      lst <- list(...)
    checkmate::assertList(lst)
    for(i in 1:length(lst)){

      if(any(is.null(lst[[i]])) | any(is.nan(lst[[i]])))
        return(stopwarn(error, paste(names(lst)[[i]],"must be a number.")))

      aid <- names(lst)[[i]]
      value <- lst[[i]]

      if(is.null(aid) | is.null(value))
        return(stopwarn(error, "Parameter names and new values must be provided."))

      param <- subset(self$as.data.table(), id == aid)

      if(nrow(param)==0)
        return(stopwarn(error, sprintf("%s is not in the parameter set.",aid)))

      if(param$support[[1]]$class() == "integer")
        value <- round(value)

      checkmate::assert(param$support[[1]]$liesInSetInterval(value, all = TRUE))

      private$.parameters[unlist(private$.parameters[,"id"]) %in% param$id, "value"][[1]] <- list(value)
    }

    self$update()

    invisible(self)
})

#' @title Combine ParameterSets
#'
#' @description merge dispatch method to combine parameter sets by rows.
#'
#' @param x ParameterSet
#' @param y ParameterSet
#' @param ... ParameterSets
#'
#' @section R6 Usage: $merge(y, ...)
#'
#' @seealso \code{\link{ParameterSet}}
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
merge.ParameterSet <- function(x, y, ...){}
ParameterSet$set("public","merge",function(y, ...){
  newsets = c(list(y), list(...))
  lapply(newsets, function(x) checkmate::assert(inherits(x,"ParameterSet"),.var.name = "All objects in merge must be ParameterSets"))

  newpar = rbind(self$as.data.table(),
                 data.table::rbindlist(lapply(newsets, function(x) x$as.data.table())))

  if(any(table(newpar$id)>1))
    stop("IDs must be unique. Try using makeUniqueDistributions first.")
  else
    private$.parameters <- newpar
  invisible(self)
})

#' @name as.data.table
#' @title Coerce ParameterSet to data.table
#'
#' @description Coerces a ParameterSet to a data.table.
#'
#' @param object ParameterSet
#'
#' @usage as.data.table(object)
#' @section R6 Usage: $as.data.table()
#'
#' @seealso \code{\link{ParameterSet}}
#'
#' @return A data.table.
#'
#' @export
NULL
ParameterSet$set("public","as.data.table",function(){
  return(private$.parameters)
})

#' @name as.ParameterSet
#' @title Coerce to a ParameterSet
#' @description Coerces objects to ParameterSet.
#' @usage as.ParameterSet(x,...)
#' @param x object
#' @param ... additional arguments
#' @details Currently supported coercions are from data tables and lists. Function assumes
#' that the data table columns are the correct inputs to a ParameterSet, see the constructor
#' for details. Similarly for lists, names are taken to be ParameterSet parameters and values taken to be
#' arguments.
#' @seealso \code{\link{ParameterSet}}
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
as.ParameterSet <- function(x,...){
  UseMethod("as.ParameterSet", x)
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.table <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, support = x$support,
                          settable = x$settable,
                          updateFunc = x$updateFunc,
                          description = x$description))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, support = x$support,
                          settable = x$settable,
                          updateFunc = x$updateFunc,
                          description = x$description))
}
