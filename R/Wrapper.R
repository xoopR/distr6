#' @name DistributionWrapper
#'
#' @title Abstract DistributionWrapper Class
#'
#' @template class_wrapper
#' @template class_abstract
#' @template method_setParameterValue
#' @template method_wrappedModels
#'
#' @details Wrappers in distr6 use the composite pattern (Gamma et al. 1994), so that a wrapped distribution
#' has the same methods and fields as an unwrapped one. After wrapping, the parameters of a distribution
#' are prefixed with the distribution name to ensure uniqueness of parameter IDs.
#'
#' Use [listWrappers] function to see constructable wrappers.
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
DistributionWrapper <- R6Class("DistributionWrapper", inherit = Distribution, lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param ... `ANY` \cr
    #' Additional arguements passed to `[Distribution]$new`
    initialize = function(distlist = NULL, ...) {
      if (getR6Class(self) == "DistributionWrapper") {
        stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))
      }

      if (!is.null(distlist)) {
        assertDistributionList(distlist)

        # lapply(distlist, function(x) x$parameters()$update())
        private$.wrappedModels <- distlist

        params <- data.table::rbindlist(lapply(distlist, function(x) {
          if (!("VectorDistribution" %in% class(x))) {
            params <- as.data.table(x[["parameters"]]())
            params[, 1] <- paste(x[["short_name"]], unlist(params[, 1]), sep = "_")
            return(params)
          }
        }))
        row.names(params) <- NULL
        if (!is.null(private$.outerParameters)) {
          params <- rbind(params, as.data.table(private$.outerParameters))
        }
        params <- as.ParameterSet(params)
      } else {
        params <- NULL
      }

      super$initialize(parameters = params, ...)
    },

    #' @description
    #' Returns model(s) wrapped by this wrapper.
    wrappedModels = function(model = NULL) {

      if (!is.null(model)) {
        if (all(model %in% names(private$.wrappedModels))) {
          if (length(model) == 1) {
            return(private$.wrappedModels[[model]])
          } else {
            return(private$.wrappedModels[model])
          }
        } else {
          private$.wrappedModels
        }
      } else {
        private$.wrappedModels
      }
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }

      for (i in 1:length(lst)) {
        if (grepl("_", names(lst)[[i]], fixed = T)) {
          id <- names(lst)[[i]]
          underscore <- gregexpr("_", id, fixed = T)[[1]][1]
          model <- substr(id, 1, underscore - 1)
          parameter <- substr(id, underscore + 1, 1000)

          newlst <- list(lst[[i]])
          names(newlst) <- parameter
          self$wrappedModels(model)$setParameterValue(lst = newlst, error = error)
        } else {
          newlst <- list(lst[[i]])
          names(newlst) <- names(lst)[[i]]
          private$.outerParameters$setParameterValue(lst = newlst)
        }
      }

      params <- data.table::rbindlist(lapply(self$wrappedModels(), function(x) {
        params <- as.data.table(x[["parameters"]]())
        params[, 1] <- paste(x[["short_name"]], unlist(params[, 1]), sep = "_")
        return(params)
      }))
      if (!is.null(private$.outerParameters)) {
        params <- rbind(params, as.data.table(private$.outerParameters))
      }
      row.names(params) <- NULL
      private$.parameters <- as.ParameterSet(params)

      invisible(self)
    }
  ),

  private = list(
    .wrappedModels = list(),
    .outerParameters = NULL
  )
)

#' @name wrappedModels
#' @title Gets Internally Wrapped Models
#' @description Returns either a list of all the wrapped models or the models named by parameters.
#'
#' @usage wrappedModels(object, model = NULL)
#'
#' @param object Distribution.
#' @param model character, see details.
#'
#' @return If \code{model} is NULL then returns list of models that are wrapped by the wrapper. Otherwise
#' returns model given in \code{model}.
#'
#' @export
NULL

