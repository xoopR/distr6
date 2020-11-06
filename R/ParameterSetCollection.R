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
    #' @param .checks Used internally.
    #' @param .supports Used internally.
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
    initialize = function(..., lst = NULL, .checks = NULL, .supports = NULL) {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if (length(lst)) {
        checkmate::assertNames(names(lst), type = "strict")
        assertParameterSetList(lst)
        private$.parametersets <- lst
        if (!is.null(.checks)) {
          private$.checks <- .checks
        }
        if (!is.null(.supports)) {
          private$.supports <- .supports
        }
      }

      invisible(self)
    },

    #' @description
    #' Prints the [ParameterSetCollection].
    #' @param hide_cols `(character())`\cr
    #' Names of columns in the [ParameterSet] to hide whilst printing.
    #' @param ... `ANY` \cr
    #' Additional arguments, currently unused.
    print = function(hide_cols = c("settable"), ...) {
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
          stopf("%s is not a parameter in this ParameterSet.", id)
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
      dt <- as.data.table(self)
      id0 <- id
      sep <- gregexpr("_", id0)[[1]][[1]]

      if (sep == -1) {
        dt = dt[grepl(paste0("_", id0, "$"), dt$id), c("id", "value")]
        if (!nrow(dt)) {
          stopf("%s is not in this ParameterSetCollection.", id)
        } else {
          lst = as.list(dt$value)
          names(lst) = unlist(strsplit(dt$id, split = paste0("_", id0)))
          return(lst)
        }
      } else {
        return(unname(unlist(subset(dt, id == id0, select = value))))
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
      if (is.null(lst)) {
        lst <- list(...)
      } else {
        checkmate::assertList(lst)
      }

      sep <- as.numeric(as.data.table(gregexpr("_", names(lst)))[1, ])
      param <- unlist(substr(names(lst), sep + 1, 1000))
      dist <- unlist(substr(names(lst), 1, sep - 1))

      .suppressCheck <- !is.null(self$checks)
      sapply(unique(dist), function(i) {
        bool <- dist == i
        newlst <- lst[bool]
        names(newlst) <- param[bool]
        private$.parametersets[[i]]$setParameterValue(lst = newlst, .suppressCheck = .suppressCheck)
      })

      if (!is.null(self$checks)) {
        private$.contains()
        private$.check()
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

      selflst <- private$.supports
      mlst <- data.table::rbindlist(lapply(lst, function(x) x$.__enclos_env__$private$.supports))
      newlst <- rbind(selflst, mlst)
      private$.supports <- newlst

      invisible(self)
    },

    #' @description
    #' Dependencies should be added to internal [ParameterSet]s.
    #' @param ... `ANY` \cr Ignored.
    addDeps = function(...) {
      stop("Dependencies should be added to internal ParameterSets with $parameterSets.")
    },

    #' @description
    #' Returns parameter set values as a named list.
    #' @param settable `(logical(1))`\cr
    #' If `TRUE` (default) only returns values of settable parameters, otherwise returns all.
    values = function(settable = TRUE) {
      rlapply(private$.parametersets, values, settable = settable)
    }
  ),

  active = list(
    #' @field deps
    #' Returns [ParameterSet] dependencies table.
    deps = function() {
      rlapply(private$.parametersets, deps, active = TRUE)
    },

    #' @field parameterSets
    #' Returns [ParameterSet]s in collection.
    parameterSets = function() {
      private$.parametersets
    }
  ),

  private = list(
    .parametersets = list(),
    .supports = data.table(),
    .contains = function() {
      apply(private$.supports, 1, function(x) {
        assertContains(x[[2]], as.Tuple(unlist(self$getParameterValue(x[[1]]))),
                       sprintf("%s does not lie in the support of %s",
                               as.Tuple(unlist(self$getParameterValue(x[[1]])))$strprint(),
                               x[[2]]$strprint()))
      })
    },
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
  paramsets <- x$.__enclos_env__$private$.parametersets

  if (length(paramsets)) {
    lst <- unlist(lapply(paramsets, function(.x) {
      r = as.data.table(.x)
      list(r, nrow(r))
    }), recursive = FALSE)

    dt <- data.table::rbindlist(lst[seq.int(1, length(lst), 2)])
    dt$id <- paste(rep(names(paramsets),
                       times = as.numeric(lst[seq.int(2, length(lst), 2)])),
                   dt$id, sep = "_")
  } else {
    dt <- data.table::data.table(id = character(), value = numeric(), support= list(),
                                 description = character())
  }

  return(dt)
}
