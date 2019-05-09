#' @title Make an R6 Parameter Set for Distributions
#'
#' @name ParameterSet
#' @description ParameterSets are passed to a \code{Distribution$new} constructor when
#'  creating a custom probability distribution that takes parameters.
#'
#' @return \code{ParameterSet$new} returns an R6 ParameterSet.
#'
#' @section Usage: ParameterSet$new(id, value, lower, upper, class, settable, fittable,
#'    updateFunc = NULL, description = NULL)
#' @param id unique identifier for parameter. See Details.
#' @param value initial value
#' @param lower minimum value parameter can take
#' @param upper maximum value parameter can take
#' @param class parameter class, one of "numeric" or "integer"
#' @param settable logical; if TRUE the parameter can be updated. See Details.
#' @param fittable logical; if TRUE the parameter can be estimated. See Details.
#' @param updateFunc string to be parsed and evaluated as function. See Details.
#' @param description description of parameter
#'
#' @details An R6 ParameterSet is required to construct a custom Probability Distribution
#'  that takes parameters. This constructor ensures that the correct format of parameters
#'  is supplied to the disitribution.
#'
#'  Each parameter requires a unique one-word \code{id} that is used to get and set parameters
#'  after construction. A parameter can be \code{settable} and \code{fittable}. A \code{settable}
#'  parameter is one that can be updated after construction of a distribution via \code{$setParameterValue}.
#'  The Distribution is parameterised by whichever parameters are given as settable.
#'  Non-settable parameters are either constant or can be automatically update if an \code{updateFunc} is
#'  provided. \code{updateFunc} should be provided as a string that could be understood in the
#'  body of a function by a Distribution object, i.e. by naming parameters via \code{$getParameterValue}, see examples.
#'  A \code{fittable} parameter is one that can be estimated via inference methods, see examples.
#'
#'  Internally after calling \code{$setParameterValue}, \code{$update} is called to update the
#'  value of non-settable functions.
#'
#' @examples
#'  id = list("prob", "size")
#'  value = list(0.2, 5)
#'  lower = list(0, 1)
#'  upper = list(1, Inf)
#'  class = list("numeric","integer")
#'  settable = list(TRUE, TRUE)
#'  fittable = list(TRUE, FALSE)
#'  description = list("Probability of success",NULL)
#'  ps = ParameterSet$new(id, value, lower, upper, class, settable, fittable,
#'     description = description)
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
#'  fittable = list(TRUE, FALSE)
#'  updateFunc = list(NULL, "1/self$getParameterValue('rate')")
#'  description = list("Arrival rate","Scale parameter")
#'  ps = ParameterSet$new(id, value, lower, upper, class, settable, fittable,
#'    updateFunc, description)
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
ParameterSet$set("public","initialize", function(id, value, lower, upper, class, settable, fittable,
                                                 updateFunc = NULL, description = NULL){

  checkmate::assert(length(id)==length(value), length(id)==length(settable),
                    length(id)==length(fittable),
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

    a_fittable  = fittable[[i]]
    checkmate::assertLogical(a_fittable, .var.name = "'fittable' must be logical")

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
                         fittable = a_fittable, description = a_description,
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
    updates = private$.parameters[!is.na(private$.parameters$updateFunc),]
    newvals = apply(updates, 1, function(x){
      fnc = function(self){}
      body(fnc) = parse(text = x[[9]])
      newval = fnc(self)
    })
    private$.parameters[!is.na(private$.parameters$updateFunc),"value"] = as.numeric(newvals)
  }

  invisible(self)
})

#' @name parameters
#' @rdname ParameterSet
#' @return \code{$parameters} either returns an R6 ParameterSet, a data.frame or a row.
#' @section Usage: $parameters(id, as.df = F)
#' @param as.df logical; if FALSE (default) parameters returned as ParameterSet, otherwise data.frame
ParameterSet$set("public","parameters",function(id, as.df = F){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(!missing(id)){
    id0 = id
    if(length(dplyr::filter(private$.parameters, id == id0))==0){
      if(as.df)
        return(private$.parameters)
      else
        return(self)
    }
    return(dplyr::filter(private$.parameters, id == id0))
  } else {
    if(as.df)
      return(private$.parameters)
    else
      return(self)
  }
}) # NEEDS TESTING

#' @name getParameterValue
#' @rdname ParameterSet
#' @return \code{$getParameterValue} returns the value of a parameter.
#' @section Usage: $getParameterValue(id)
ParameterSet$set("public","getParameterValue",function(id){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  val = self$parameters(id = id, as.df = T)[["value"]]
  if(length(val)==0)
    return(paste(id, "is not a parameter in this distribution."))
  else
    return(val[[1]])
}) # NEEDS TESTING

#' @name setParameterValue
#' @rdname ParameterSet
#' @return \code{$setParameterValue} returns ParameterSet invisibly.
#' @section Usage: $setParameterValue(lst)
#' @param lst list; list names are parameter IDs, list values are values to set parameters
ParameterSet$set("public","setParameterValue",function(lst){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")
  checkmate::assertList(lst)

  for(i in 1:length(lst)){
    id <- names(lst)[[i]]
    value <- lst[[i]]

    param <- self$parameters(as.df = T)[self$parameters(as.df = T)[,"id"] %in% id,]
    if(length(param)==0)
      stop(sprintf("%s is not in the parameter set.",id))

    if(!param$settable)
      stop(sprintf("%s is not settable.",param$name))

    if(param$class=="numeric")
      checkmate::assertNumeric(value,lower = param$lower, upper = param$upper)
    if(param$class=="integer"){
      value = as.integer(value)
      checkmate::assertInteger(value,lower = param$lower, upper = param$upper)
    }
    private$.parameters[private$.parameters[,"id"] %in% param$id, "value"] <- value
  }

  self$update()

  invisible(self)
}) # NEEDS TESTING

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
                          fittable = x$fittable, updateFunc = x$updateFunc,
                          description = x$description))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.table <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, lower = x$lower,
                          upper = x$upper, class = x$class, settable = x$settable,
                          fittable = x$fittable, updateFunc = x$updateFunc,
                          description = x$description))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x,...){
  return(ParameterSet$new(id = x$id, value = x$value, lower = x$lower,
                          upper = x$upper, class = x$class, settable = x$settable,
                          fittable = x$fittable, updateFunc = x$updateFunc,
                          description = x$description))
}
