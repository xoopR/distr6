#' @title Parameter Sets for Distributions
#'
#' @template method_setParameterValue
#' @template method_getParameterValue
#'
#' @description
#' ParameterSets are passed to the [Distribution] constructor when
#'  creating a custom probability distribution that takes parameters.
#'
#' @export
ParameterSet <- R6Class("ParameterSet",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @details
    #' Every argument can either be given as the type listed or as a list of that type.
    #' If arguments are provided as a list, then each argument must be of the same length,
    #' with values as NULL where appropriate. See examples for more.
    #'
    #' @param id `(character(1)|list())`\cr
    #' id of the parameter(s) to construct, should be unique.
    #' @param value `(ANY|list())`\cr
    #' Value of parameter(s) to set.
    #' @param support `([set6::Set]|list())`\cr
    #' Support of parameter(s) to set
    #' @param settable `(character(1)|list())`\cr
    #' Logical flag indicating if the parameter(s) can be updated after construction.
    #' @param updateFunc `(function(1)|list())`\cr
    #' Optional function to update the parameter(s) based on other parameter values.
    #' These should be given as a function that could be understood in the body of a [Distribution]
    #' and should start with `function(self)`, see examples.
    #' @param description `(character(1)|list())`\cr
    #' Optional description for the parameter(s).
    #'
    #' @examples
    #' id <- list("prob", "size")
    #' value <- list(0.2, 5)
    #' support <- list(set6::Interval$new(0, 1), set6::PosNaturals$new())
    #' settable <- list(TRUE, TRUE)
    #' description <- list("Probability of success", NULL)
    #' updateFunc <- NULL
    #' ParameterSet$new(id = id,
    #'                  value = value,
    #'                  support = support,
    #'                  settable = settable,
    #'                  updateFunc = updateFunc,
    #'                  description = description
    #'  )
    #'
    #' ParameterSet$new(id = "prob",
    #'                  value = 0.2,
    #'                  support = set6::Interval$new(0, 1),
    #'                  settable = TRUE,
    #'                  description = "Probability of success"
    #'  )
    #'
    #' id <- list("rate", "scale")
    #' value <- list(1, 1)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' settable <- list(TRUE, FALSE)
    #' updateFunc <- list(NULL, function(self) 1 / self$getParameterValue("rate"))
    #' description <- list("Arrival rate", "Scale parameter")
    #' ps <- ParameterSet$new(
    #'   id, value, support, settable,
    #'   updateFunc, description
    #' )
    initialize = function(id, value, support, settable,
                          updateFunc = NULL, description = NULL) {

      # coerce all to lists except id, and settable (should be same type)
      id <- unlist(id)
      if (!checkmate::testList(value)) {
        value <- if (length(value) > 1) as.list(value) else list(value)
      }
      if (!checkmate::testList(support)) support <- list(support)
      settable <- unlist(settable)

      # check lengths
      checkmate::assert(length(unique(length(id), length(value), length(settable), length(support))) == 1,
                        .var.name = "arguments of same length"
      )

      # id checks
      checkmate::assert(!any(duplicated(id)), .var.name = "'id's must be unique")
      assertOneWord(id)

      # support checks
      assertSetList(support)

      # settable checks
      checkmate::assertLogical(settable)

      # value checks
      mapply(function(x, y) {
        if (length(x) > 1) {
          assertContains(y, Tuple$new(x), paste(strCollapse(x, "()"), "does not lie in the set", y$strprint()))
        } else {
          assertContains(y, x, paste(x, "does not lie in the set", y$strprint()))
        }
      }, value, support)

      # description checks
      if (!is.null(description)) {
        checkmate::assert(length(id) == length(description), .var.name = "arguments of same length")
        description[sapply(description, is.null)] <- "None"
        description <- unlist(description)
        checkmate::assertCharacter(description, null.ok = TRUE)
      }

      # update checks
      if (!is.null(updateFunc)) {
        if (!checkmate::testList(updateFunc)) updateFunc <- list(updateFunc)
        checkmate::assert(length(id) == length(updateFunc), .var.name = "arguments of same length")
        sapply(updateFunc, checkmate::assertFunction, null.ok = TRUE)
      }

      private$.parameters <- data.table(
        id = id, value = value, support = support, settable = settable,
        description = description, updateFunc = updateFunc
      )
      invisible(self)
    },

    #' @description
    #' Prints the [ParameterSet].
    #' @param hide_cols `(character())`\cr
    #' Names of columns in the [ParameterSet] to hide whilst printing.
    #' @param ... \cr
    #' Additional arguments, currently unused.
    print = function(hide_cols = c("updateFunc", "settable"), ...) {
      ps <- private$.parameters
      ps$support <- lapply(ps$support, function(x) x$strprint())
      print(subset(ps, select = !(names(ps) %in% hide_cols)))
    },

    #' @description
    #' Returns the full parameter details for the supplied parameter.
    #' @param id `character()` \cr
    #' id of parameter to return.
    parameters = function(id = NULL) {
      if (!is.null(id)) {
        id0 <- id
        if (nrow(subset(private$.parameters, id %in% id0)) == 0) {
          return(self)
        } else {
          return(subset(private$.parameters, id %in% id0))
        }
      } else {
        return(self)
      }
    },

    #' @description
    #' Returns the support of the supplied parameter.
    #' @param id `character()` \cr
    #' id of parameter support to return.
    #' @return
    #' A [set6::Set] object.
    #'
    #' @examples
    #' ps <- ParameterSet$new(id = "prob",
    #'                  value = 0.2,
    #'                  support = set6::Interval$new(0, 1),
    #'                  settable = TRUE,
    #'                  description = "Probability of success"
    #'  )
    #' ps$getParameterSupport("prob")
    getParameterSupport = function(id, error = "warn") {
      if (missing(id)) {
        return(stopwarn(error, "Argument 'id' is missing, with no default."))
      }

      support <- self$parameters(id)[["support"]]
      if (length(support) == 0) {
        return(stopwarn(error, paste(id, "is not a parameter in this distribution.")))
      } else {
        return(unlist(support[[1]]))
      }

    },

    #' @description
    #' Returns the value of the supplied parameter.
    #' @return
    #' A [set6::Set] object.
    #'
    #' @examples
    #' ps <- ParameterSet$new(id = "prob",
    #'                  value = 0.2,
    #'                  support = set6::Interval$new(0, 1),
    #'                  settable = TRUE,
    #'                  description = "Probability of success"
    #'  )
    #' ps$getParameterValue("prob")
    getParameterValue = function(id, error = "warn") {
      if (missing(id)) {
        return(stopwarn(error, "Argument 'id' is missing, with no default."))
      }
      val <- try(self$parameters(id)[["value"]], silent = T)
      if (class(val) == "try-error" | length(val) == 0) {
        return(stopwarn(error, paste(id, "is not a parameter in this distribution.")))
      } else {
        return(val[[1]])
      }

    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    #'
    #' @examples
    #' id <- list("rate", "scale")
    #' value <- list(1, 1)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' settable <- list(TRUE, FALSE)
    #' updateFunc <- list(NULL, function(self) 1 / self$getParameterValue("rate"))
    #' description <- list("Arrival rate", "Scale parameter")
    #' ps <- ParameterSet$new(
    #'   id, value, support, settable,
    #'   updateFunc, description
    #' )
    #' ps$getParameterValue(id = "rate")
    #' ps$setParameterValue(rate = 2)
    #' ps$getParameterValue("scale") # Auto-updated to 1/2
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      checkmate::assertList(lst)
      for (i in 1:length(lst)) {

        if (any(is.null(lst[[i]])) | any(is.nan(lst[[i]]))) {
          return(stopwarn(error, paste(names(lst)[[i]], "must be a number.")))
        }

        aid <- names(lst)[[i]]
        value <- lst[[i]]

        if (is.null(aid) | is.null(value)) {
          return(stopwarn(error, "Parameter names and new values must be provided."))
        }

        param <- subset(as.data.table(self), id == aid)

        if (nrow(param) == 0) {
          return(stopwarn(error, sprintf("%s is not in the parameter set.", aid)))
        }

        # if(param$support[[1]]$class() == "integer")
        #   value <- round(value)

        if (length(value) > 1) {
          if (!param$support[[1]]$contains(Tuple$new(value), all = TRUE)) {
            stop(Tuple$new(value)$strprint(), " does not lie in the support of parameter ", aid)
          }
        } else {
          if (!param$support[[1]]$contains(value, all = TRUE)) {
            stop(value, " does not lie in the support of parameter ", aid)
          }
        }


        private$.parameters[unlist(private$.parameters[, "id"]) %in% param$id, "value"] <- list(value)
      }

      private$.update()

      invisible(self)
    },

    #' @description
    #' Merges multiple parameter sets.
    #' @param y [ParameterSet]
    #' @param ... [ParameterSet]s
    #' @examples
    #' ps1 <- ParameterSet$new(id = "prob",
    #'                  value = 0.2,
    #'                  support = set6::Interval$new(0, 1),
    #'                  settable = TRUE,
    #'                  description = "Probability of success"
    #'  )
    #'  ps2 <- ParameterSet$new(id = "size",
    #'                  value = 10,
    #'                  support = set6::Interval$new(0, 10, class = "integer"),
    #'                  settable = TRUE,
    #'                  description = "Number of trials"
    #'  )
    #'  ps1$merge(ps2)$print()
    merge = function(y, ...) {
      newsets <- c(list(y), list(...))
      lapply(newsets, function(x) checkmate::assert(inherits(x, "ParameterSet"), .var.name = "All objects in merge must be ParameterSets"))

      newpar <- rbind(
        as.data.table(self),
        data.table::rbindlist(lapply(newsets, function(x) as.data.table(x)))
      )

      if (any(table(newpar$id) > 1)) {
        stop("IDs must be unique. Try using makeUniqueDistributions first.")
      } else {
        private$.parameters <- newpar
      }
      invisible(self)
    }
  ),

  private = list(
    .parameters = NULL,
    .setParameterSupport = function(lst) {
      id <- names(lst)
      support <- lst[[1]]
      which <- which(unlist(private$.parameters$id) %in% id)
      private$.parameters[which, 3][[1]] <- list(support)
      invisible(self)
    },
    .update = function(...) {
      sap <- sapply(private$.parameters$updateFunc, is.null)
      if (any(!sap)) {
        update_filter <- !sapply(private$.parameters$updateFunc, is.null)
        updates <- private$.parameters[update_filter, ]
        newvals <- apply(updates, 1, function(x) {
          return(x[[6]](self))
          # if(length(newval) > 1) {
          #   if(!x[[3]]$contains(Tuple$new(newval)))
          #     stop(Tuple$new(newval)$strprint(), " does not lie in the support of parameter ", x[[1]])
          # } else {
          #   if(!x[[3]]$contains(newval))
          #     stop(newval, " does not lie in the support of parameter ", x[[1]])
          # }

          # return(newval)
        })
        suppressWarnings(data.table::set(private$.parameters, which(update_filter), "value", as.list(newvals)))
      }

      invisible(self)
    }
  )
)

#' @name print.ParameterSet
#' @title Print a ParameterSet
#'
#' @description Prints a ParameterSet as a data.table with strprint variants of R6 classes.
#'
#' @param x ParameterSet
#' @param hide_cols string, if given the data.table is filtered to hide these columns
#' @param ... ignored, added for S3 consistency
#'
#' @export
print.ParameterSet <- function(x, hide_cols, ...) {}

#' @name parameters
#' @title Parameters Accessor
#' @description Returns some or all the parameters in a distribution.
#'
#' @usage parameters(object, id = NULL)
#'
#' @param object Distribution or ParameterSet.
#' @param id character, see details.
#'
#' @return An R6 object of class ParameterSet or a data.table.
#'
#' @export
NULL

#' @name getParameterSupport
#' @title Parameter Support Accessor
#' @description Returns the support of the given parameter.
#' @usage getParameterSupport(object, id, error = "warn")
#'
#' @param object Distribution or ParameterSet.
#' @param id character, id of the parameter to return.
#' @param error character, value to pass to \code{stopwarn}.
#'
#' @return An R6 object of class inheriting from [set6::Set]
#'
#' @export
NULL

#' @name getParameterValue
#' @title Parameter Value Accessor
#' @description Returns the value of the given parameter.
#' @usage getParameterValue(object, id, error = "warn")
#'
#' @param object Distribution or ParameterSet.
#' @param id character, id of the parameter to return.
#' @param error character, value to pass to \code{stopwarn}.
#'
#' @return The current value of a given parameter as a numeric.
#'
#' @export
NULL

#' @name setParameterValue
#' @title Parameter Value Setter
#' @description Sets the value of the given parameter.
#'
#' @usage setParameterValue(object, ..., lst = NULL, error = "warn")
#'
#' @param object Distribution or ParameterSet.
#' @param ... named parameters and values to update, see details.
#' @param lst optional list, see details.
#' @param error character, value to pass to \code{stopwarn}.
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
NULL

#' @title Combine ParameterSets
#'
#' @param x ParameterSet
#' @param y ParameterSet
#' @param ... ParameterSets
#'
#' @return An R6 object of class ParameterSet.
#'
#' @export
merge.ParameterSet <- function(x, y, ...) {}

#' @title Coerce ParameterSet to data.table
#'
#' @description Coerces a ParameterSet to a data.table.
#'
#' @param x ParameterSet
#' @param ... Ignored.
#'
#' @method as.data.table ParameterSet
#'
#' @return A data.table.
#'
#' @export
as.data.table.ParameterSet <- function(x, ...) {
  x$.__enclos_env__$private$.parameters
}

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
as.ParameterSet <- function(x, ...) {
  UseMethod("as.ParameterSet", x)
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.data.table <- function(x, ...) {
  return(ParameterSet$new(
    id = x$id, value = x$value, support = x$support,
    settable = x$settable,
    updateFunc = x$updateFunc,
    description = x$description
  ))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x, ...) {
  return(ParameterSet$new(
    id = x$id, value = x$value, support = x$support,
    settable = x$settable,
    updateFunc = x$updateFunc,
    description = x$description
  ))
}
