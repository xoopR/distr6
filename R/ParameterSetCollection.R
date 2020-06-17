#' @title Parameter Set Collections for Wrapped Distributions
#'
#' @template method_setParameterValue
#' @template param_paramid
#'
#' @description
#' ParameterSetCollection is used to combine multiple [ParameterSet]s in wrapped
#'   distributions. Generally only need to be constructed internally.
#'
#' @export
ParameterSetCollection <- R6Class("ParameterSetCollection",
  inherit = ParameterSet,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param ... `([ParameterSet])`\cr
    #' [ParameterSet]s to combine into a collection. Should be supplied as named arguments
    #' where the names are unique and correspond to references for the distributions.
    #' @param lst `(list())`\cr
    #' Alternative constructor by supplying a named list of [ParameterSet]s.
    #' @examples
    #' b = Binomial$new()
    #' g = Geometric$new()
    #' ParameterSetCollection$new(Binom1 = b$parameters(),
    #'                            Binom2 = b$parameters(),
    #'                            Geom = g$parameters())
    #'
    #' ParameterSetCollection$new(lst = list(Binom1 = b$parameters(),
    #'                                       Binom2 = b$parameters(),
    #'                                       Geom = g$parameters()))
    initialize = function(..., lst = NULL) {
      if (is.null(lst)) {
        lst <- list(...)
      }
      checkmate::assertNames(names(lst), type = "strict")
      assertParameterSetList(lst)
      private$.parametersets <- lst
      invisible(self)
    },

    #' @description
    #' Prints the [ParameterSetCollection].
    #' @param hide_cols `(character())`\cr
    #' Names of columns in the [ParameterSet] to hide whilst printing.
    #' @param ... \cr
    #' Additional arguments, currently unused.
    print = function(hide_cols = c("updateFunc", "settable"), ...) {
      psc <- as.data.table(self)
      psc$support <- lapply(psc$support, function(x) x$strprint())
      print(subset(psc, select = !(names(psc) %in% hide_cols)))
    },

    #' @description
    #' Returns the full parameter details for the supplied parameter, or returns `self`
    #' if `id` is `NULL` or unmatched.
    #' @param id `character()` \cr
    #' id of parameter to return.
    parameters = function(id = NULL) {
      dt <- as.data.table(self)
      if (!is.null(id)) {
        id0 <- id
        if (nrow(subset(dt, id %in% id0)) == 0) {
          return(self)
        } else {
          return(subset(dt, id %in% id0))
        }
      } else {
        return(self)
      }
    },

    #' @description
    #' Returns the value of the supplied parameter.
    #' @param id `(character(1))`
    #' To return the parameter for a specific distribution, use the parameter ID with the
    #' distribution name prefix, otherwise to return the parameter for all distributions omit
    #' the prefix. See examples.
    #'
    #' @examples
    #' psc <- ParameterSetCollection$new(Binom1 = Binomial$new()$parameters(),
    #'                                   Binom2 = Binomial$new()$parameters(),
    #'                                   Geom = Geometric$new()$parameters())
    #' psc$getParameterValue("Binom1_prob")
    #' psc$getParameterValue("prob")
    getParameterValue = function(id, error = "warn") {
      sep <- gregexpr("_", id)[[1]][[1]]

      if (sep == -1) {
        return(lapply(private$.parametersets, function(x) x$getParameterValue(id)))
      } else {
        param <- substr(id, sep + 1, 1000)
        dist <- substr(id, 1, sep - 1)
        return(private$.parametersets[[dist]]$getParameterValue(param, error = error))
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
    #' b <- Binomial$new()
    #' g <- Geometric$new()
    #' psc <- ParameterSetCollection$new(Binom1 = b$parameters(),
    #'                                   Binom2 = b$parameters(),
    #'                                   Geom = g$parameters())
    #' psc$getParameterSupport("Binom1_prob")
    getParameterSupport = function(id, error = "warn") {
      param <- strsplit(id, "_", fixed = TRUE)[[1]]
      private$.parametersets[[param[1]]]$getParameterSupport(param[2], error = error)
    },

    #' @description
    #' Sets the value(s) of the given parameter(s). Because of R6 reference semantics
    #' this also updates the [ParameterSet] of the wrapped distibution, and vice versa.
    #' See examples.
    #'
    #' @examples
    #' b <- Binomial$new()
    #' g <- Geometric$new()
    #' psc <- ParameterSetCollection$new(Binom1 = b$parameters(),
    #'                                   Binom2 = b$parameters(),
    #'                                   Geom = g$parameters())
    #' psc$getParameterValue("Binom1_prob")
    #' b$getParameterValue("prob")
    #' psc$setParameterValue(Binom1_prob = 0.4)
    #' # both updated
    #' psc$getParameterValue("Binom1_prob")
    #' b$getParameterValue("prob")
    #'
    #' g$setParameterValue(prob = 0.1)
    #' # both updated
    #' psc$getParameterValue("Geom_prob")
    #' g$getParameterValue("prob")
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      sep <- as.numeric(as.data.table(gregexpr("_", names(lst)))[1, ])
      param <- substr(names(lst), sep + 1, 1000)
      dist <- substr(names(lst), 1, sep - 1)

      for (i in unique(dist)) {
        newlst <- lst[dist == i]
        names(newlst) <- unlist(param)[dist == i]
        private$.parametersets[[i]]$setParameterValue(lst = newlst)
      }

      invisible(self)
    },

    #' @description
    #' Merges other [ParameterSetCollection]s into `self`.
    #' @param ... `([ParameterSetCollection]s)`
    #' @param `lst` `(list())` \cr
    #' Alternative method of passing a list of [ParameterSetCollection]s.
    #' @examples
    #' b <- Binomial$new()
    #' g <- Geometric$new()
    #' psc <- ParameterSetCollection$new(Binom = b$parameters())
    #' psc2 <- ParameterSetCollection$new(Geom = g$parameters())
    #' psc$merge(psc2)$parameters()
    #'
    merge = function(..., lst = NULL) {
      if (is.null(lst)) lst <- list(...)
      assertParameterSetCollectionList(lst)
      selflst <- private$.parametersets
      mlst <- unlist(lapply(lst, function(x) x$.__enclos_env__$private$.parametersets))
      newlst <- c(selflst, mlst)
      checkmate::assertNames(names(newlst), "strict")
      private$.parametersets <- newlst
      invisible(self)
    },

    #' @description
    #' Dependencies should be added to internal [ParameterSet]s.
    #' @param ... \cr
    #' Ignored.
    addDeps = function(...) {
      stop("Dependencies should be added to internal ParameterSets with $parameterSets.")
      }
    ),

  active = list(
    #' @field values
    #' Returns parameter set values as a named list.
    values = function() {
      rlapply(private$.parametersets, values, active = TRUE)
    },

    #' @field deps
    #' Returns [ParameterSet] dependencies table.
    deps = function() {
      rlapply(private$.parametersets, deps, active = TRUE)
    },

    #' @field parameterSets
    #' Returns [ParameterSet]s in collection.
    parameterSets = function () {
      private$.parametersets
    }
  ),

  private = list(
    .parametersets = list(),
    deep_clone = function(name, value) {
      if (name %in% c(".parametersets")) {
        lapply(value, function(x) x$clone(deep = TRUE))
      } else {
        value
      }
    }
  )
)

#' @method as.data.table ParameterSetCollection
#' @export
as.data.table.ParameterSetCollection <- function(x, ...) {
  dt <- data.table(id = NULL, value = NULL, support = NULL)
  paramsets <- x$.__enclos_env__$private$.parametersets
  for (i in seq_along(paramsets)) {
    ps <- as.data.table(paramsets[[i]])
    ps$id <- paste(names(paramsets)[[i]], ps$id, sep = "_")
    dt <- rbind(dt, ps)
  }

  return(dt)
}
