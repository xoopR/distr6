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
#' \code{lower} \tab numeric \tab minimum value parameter can take. \cr
#' \code{upper} \tab numeric \tab maximum value parameter can take. \cr
#' \code{class} \tab character \tab parameter class; "numeric" or "integer". \cr
#' \code{settable} \tab logical \tab if TRUE the parameter can be updated. See Details. \cr
#' \code{updateFunc = NULL} \tab character \tab string to be parsed and evaluated as function. See Details. \cr
#' \code{description = NULL} \tab character \tab optional description of parameter.
#' }
#'
#' @section Constructor Details:
#' An R6 ParameterSet is required to construct a custom Probability Distribution that takes parameters.
#' This constructor ensures that the correct format of parameters is supplied to the disitribution.
#'
#' Every argument can either be given as the correct type (as listed above) or as a list of that type.
#' If arguments are provided as a list, then each argument must be of the same length list, with values
#' as NULL where appropriate. See examples for more.
#'
#' Each parameter requires a unique one-word \code{id} that is used to get and set parameters
#' after construction. A \code{settable} parameter is one that can be updated after construction of
#' a distribution via \code{$setParameterValue}. The Distribution is parameterised by whichever parameters
#' are given as \code{settable}. Non-settable parameters are either constant or can be automatically updated
#' if an \code{updateFunc} is provided. \code{updateFunc} should be provided as a string that could be
#' understood in the body of a function by a Distribution object, i.e. by naming parameters via
#' \code{$getParameterValue}, see examples.
#'
#' Internally after calling \code{$setParameterValue}, \code{$update} is called to update the
#' value of non-settable functions.
#'
#'@section Public Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Details} \cr
#'   \code{print()} \tab Print ParameterSet as data.frame. \cr
#'   \code{update()} \tab Updates unsettable parameters with supplied update functions. \cr
#'   \code{parameters(id, error = "warn")} \tab If id given, returns specific parameter. Otherwise returns self. \cr
#'   \code{getParameterValue(id, error = "warn")} \tab Returns value of parameter matching gven 'id'. \cr
#'   \code{setParameterValue(lst, error = "warn")} \tab Set parameters in list names with respective values. See Details. \cr
#'   \code{rbind()} \tab Combine the rows of multiple ParameterSets. \cr
#'   \code{as.data.frame()} \tab Coerces ParameterSet to data.frame.
#' }
#'
#' @section Public Method Details:
#' Argument 'error' is passed to \code{\link{stopwarn}} to determine if the code should break or if a
#' warning should be returned when an error occurs.
#'
#' \code{setParameterValue} takes a named list where the list names, \code{names(lst)}, should match
#' the parameter IDs and the values, \code{as.numeric(lst)}, are used to set the corresponding parameter value.
#'
#' @examples
#'  id = list("prob", "size")
#'  value = list(0.2, 5)
#'  lower = list(0, 1)
#'  upper = list(1, Inf)
#'  class = list("numeric","integer")
#'  settable = list(TRUE, TRUE)
#'  description = list("Probability of success",NULL)
#'  ps = ParameterSet$new(id, value, lower, upper, class, settable,
#'                        description = description)
#'  ps$parameters()
#'  ps$getParameterValue("prob")
#'
#'
#' @examples
#'  id = list("rate", "scale")
#'  value = list(1, 1)
#'  lower = list(0, 0)
#'  upper = list(Inf, Inf)
#'  class = list("numeric","numeric")
#'  settable = list(TRUE, FALSE)
#'  updateFunc = list(NULL, "1/self$getParameterValue('rate')")
#'  description = list("Arrival rate","Scale parameter")
#'  ps = ParameterSet$new(id, value, lower, upper, class, settable,
#'                        updateFunc, description)
#'  ps$parameters(id = "rate")
#'  ps$setParameterValue(list(rate = 2)) # Automatically calls $update
#'  ps$getParameterValue("scale") # Auto-updated to 1/2
#'
#' @seealso \code{\link{Distribution}}
#'
#' @export
NULL

ParameterSet <- R6::R6Class("ParameterSet")
ParameterSet$set("private",".parameters",NULL)
ParameterSet$set("public","initialize", function(id, value, lower, upper, class, settable,
                                                 updateFunc = NULL, description = NULL){

  checkmate::assert(length(id)==length(value), length(id)==length(settable),
                    length(id)==length(class), length(id)==length(lower),
                    length(id)==length(upper), combine = "and",
                    .var.name = "all arguments must be of same length")
  if(!is.null(description))
    checkmate::assert(length(id)==length(description), .var.name = "all arguments must be of same length")
  if(!is.null(updateFunc))
    checkmate::assert(length(id)==length(updateFunc), .var.name = "all arguments must be of same length")
  checkmate::assert(!any(duplicated(id)), .var.name = "'id's must be unique")

  params = data.frame()
  for(i in 1:length(id)){
    a_id = id[[i]]
    checkmate::assertCharacter(a_id,.var.name = "'id' must be character")
    checkmate::assert(length(strsplit(a_id," ",fixed=T)[[1]])==1,
                      .var.name = "'id' must be one word")

    a_settable  = settable[[i]]
    checkmate::assertLogical(a_settable, .var.name = "'settable' must be logical")

    a_class = class[[i]]
    checkmate::assert(a_class == "numeric", a_class == "integer",
                      .var.name = "'class' must be one of: 'numeric' or 'integer'")

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

    a_lower = ifelse(lower[[i]] == -Inf, lower[[i]], as(lower[[i]], a_class))
    a_upper = ifelse(upper[[i]] == Inf, upper[[i]], as(upper[[i]], a_class))

    a_value = as(value[[i]], a_class)
    checkmate::assert(a_value >= a_lower, a_value <= a_upper, combine = "and",
                      .var.name = "'value' should be between 'lower' and 'upper'")

    a_param = data.frame(id = a_id, value = a_value, lower = a_lower,
                         upper = a_upper, class = a_class, settable = a_settable,
                         description = a_description,
                         updateFunc = a_update,
                         stringsAsFactors = F)

    params = rbind(params, a_param)
  }

  private$.parameters <- params
  invisible(self)
})
ParameterSet$set("public","print", function(){
  print(private$.parameters)
})
ParameterSet$set("public","update", function(){
  if(any(!is.na(private$.parameters$updateFunc))){
    update_filter = !is.na(private$.parameters$updateFunc) & !private$.parameters$settable
    updates = private$.parameters[update_filter,]
    newvals = apply(updates, 1, function(x){
      fnc = function(self){}
      body(fnc) = parse(text = x[[8]])
      newval = fnc(self)
    })
    private$.parameters[update_filter,"value"] = as.numeric(newvals)
  }
  invisible(self)
})
ParameterSet$set("public","parameters",function(id = NULL, error = "warn"){
  if(length(private$.parameters)==0)
    stopwarn(error, "There are no parameters in this distribution.")

  if(!is.null(id)){
    id0 = id
    if(length(dplyr::filter(private$.parameters, id == id0))==0){
      return(self)
    }
    return(dplyr::filter(private$.parameters, id == id0))
  } else {
      return(self)
  }
})
ParameterSet$set("public","getParameterValue",function(id, error = "warn"){

  if(length(private$.parameters)==0)
    stopwarn(error, "There are no parameters in this distribution.")
  if(missing(id))
    stopwarn(error, "Argument 'id' is missing, with no default.")
  val = self$parameters(id, TRUE)[["value"]]
  if(length(val)==0){
    stopwarn(error, paste(id, "is not a parameter in this distribution."))
  }else
    return(val[[1]])

}) # NEEDS TESTING
ParameterSet$set("public","setParameterValue",function(lst, error = "warn"){
  if(length(private$.parameters)!=0){

    checkmate::assertList(lst)

    for(i in 1:length(lst)){
      id <- names(lst)[[i]]
      value <- lst[[i]]

      param <- self$as.data.frame()[self$as.data.frame()[,"id"] %in% id,]

      if(nrow(param)==0)
        stopwarn(error, sprintf("%s is not in the parameter set.",id))

      if(!param$settable)
        stopwarn(error, sprintf("%s is not settable.",param$id))

      if(param$class=="numeric")
        checkmate::assertNumeric(value,lower = param$lower, upper = param$upper)
      if(param$class=="integer"){
        value = as.integer(value)
        checkmate::assertInteger(value,lower = param$lower, upper = param$upper)
      }

      private$.parameters[private$.parameters[,"id"] %in% param$id, "value"] <- value
    }

    rm(id, value, i)
    self$update()

    invisible(self)
  }
}) # NEEDS TESTING
ParameterSet$set("public","rbind",function(...){
  newpar = rbind(self$as.data.frame(),
                 do.call(rbind,lapply(list(...), function(x) x$as.data.frame())))
  if(any(table(newpar$id)>1))
    stop("IDs must be unique. Try using makeUniqueDistributions first.")
  else
    return(as.ParameterSet(newpar))
})
ParameterSet$set("public","as.data.frame",function(){
  return(private$.parameters)
})

#' @name as.ParameterSet
#' @title Coerce to a ParameterSet
#' @description Coerces objects to ParameterSet.
#' @return ParameterSet
#' @usage as.ParameterSet(x,...)
#' @param x object
#' @param ... additional arguments
#' @details Currently supported coercions are from data frames, data tables and lists. Function assumes
#' that the data frame and data table columns are the correct inputs to a ParameterSet, see the constructor
#' for details. Similarly for lists, names are taken to be ParameterSet parameters and values taken to be
#' arguments.
#' @seealso \code{\link{ParameterSet}}
#'
#' @export
as.ParameterSet <- function(x,...){
  UseMethod("as.ParameterSet", x)
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.frame <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, lower = x$lower,
                          upper = x$upper, class = x$class, settable = x$settable,
                          updateFunc = x$updateFunc,
                          description = x$description))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.table <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, lower = x$lower,
                          upper = x$upper, class = x$class, settable = x$settable,
                          updateFunc = x$updateFunc,
                          description = x$description))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, lower = x$lower,
                          upper = x$upper, class = x$class, settable = x$settable,
                          updateFunc = x$updateFunc,
                          description = x$description))
}
