#' @title Parameter Sets for Distributions
#'
#' @template method_setParameterValue
#' @template param_paramid
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
    #' @param updateFunc \cr
    #' Deprecated, please use `$addDeps` instead.
    #' @param description `(character(1)|list())`\cr
    #' Optional description for the parameter(s).
    #'
    #' @examples
    #' id <- list("prob", "size")
    #' value <- list(0.2, 5)
    #' support <- list(set6::Interval$new(0, 1), set6::PosNaturals$new())
    #' description <- list("Probability of success", NULL)
    #' ParameterSet$new(id = id,
    #'                  value = value,
    #'                  support = support,
    #'                  description = description
    #'  )
    #'
    #' ParameterSet$new(id = "prob",
    #'                  value = 0.2,
    #'                  support = set6::Interval$new(0, 1),
    #'                  description = "Probability of success"
    #'  )
    initialize = function(id, value, support, settable = TRUE,
                          updateFunc = NULL,
                          description = NULL) {

      if (!is.null(updateFunc)) {
        warning("updateFunc is now deprecated, please use $addDeps instead.")
      }

      # coerce all to lists except id, and settable (should be same type)
      id <- unlist(id)
      if (!checkmate::testList(value)) {
        value <- if (length(value) > 1) as.list(value) else list(value)
      }
      if (!checkmate::testList(support)) support <- list(support)
      settable <- unlist(settable)
      if (length(settable) == 1) settable <- rep(settable, length(id))

      # check lengths
      checkmate::assert(length(unique(length(id), length(value), length(settable),
                                      length(support))) == 1,
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
          assertContains(y, Tuple$new(x), paste(strCollapse(x, "()"), "does not lie in the set",
                                                y$strprint()))
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

      private$.parameters <- data.table(
        id = id, value = value, support = support, settable = settable,
        description = description
      )
      invisible(self)
    },

    #' @description
    #' Prints the [ParameterSet].
    #' @param hide_cols `(character())`\cr
    #' Names of columns in the [ParameterSet] to hide whilst printing.
    #' @param ... \cr
    #' Additional arguments, currently unused.
    print = function(hide_cols = c("settable"), ...) {
      ps <- private$.parameters
      ps$support <- lapply(ps$support, function(x) x$strprint())
      print(subset(ps, select = !(names(ps) %in% hide_cols)))
    },

    #' @description
    #' Returns the full parameter details for the supplied parameter, or returns `self`
    #' if `id` is `NULL`.
    #' @param id `character()` \cr
    #' id of parameter to return.
    parameters = function(id = NULL) {
      if (!is.null(id)) {
        id0 <- id
        if (nrow(subset(private$.parameters, id %in% id0)) == 0) {
          stopf("%s is not a parameter in this ParameterSet.", id)
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

      self$parameters(id)[["support"]][[1]]
    },

    #' @description
    #' Returns the value of the supplied parameter.
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

      self$parameters(id)[["value"]][[1]]
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    #'
    #' @examples
    #' id <- list("rate", "scale")
    #' value <- list(1, 1)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' settable <- list(TRUE, FALSE)
    #' ps <- ParameterSet$new(
    #'   id, value, support, settable,
    #' )
    #' ps$addDeps("scale", "rate", function(self) 1 / self$getParameterValue("scale"))
    #' ps$addDeps("rate", "scale", function(self) 1 / self$getParameterValue("rate"))
    #' ps$getParameterValue(id = "rate")
    #' ps$setParameterValue(rate = 2)
    #' ps$getParameterValue("rate")
    #' ps$getParameterValue("scale") # Auto-updated to 1/2
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      checkmate::assertList(lst)
      orig <- data.table::copy(private$.parameters) # FIXME - Hacky fix for checks

      # for-loop is non-signif faster than apply (63 vs 92 ns)
      for (i in seq_along(lst)) {
        value <- lst[[i]]

        if (!is.null(value)) {

          aid <- names(lst)[[i]]
          if (is.null(aid)) {
            return(stopwarn(error, "Parameter names and new values must be provided."))
          }

          param <- subset(as.data.table(self), id == aid)

          if (nrow(param) == 0) {
            return(stopwarn(error, sprintf("'%s' is not in the parameter set.", aid)))
          }

          if (!param$settable) {
            return(stopwarn(error, sprintf("'%s' is not (manually) settable after construction.",
                                           aid)))
          }

          value <- private$.trafo(aid, value)
          # private$.check(aid, value)

          if (length(value) > 1) {
            if (!param$support[[1]]$contains(Tuple$new(value), all = TRUE)) {
              stopf("%s does not lie in the support of '%s'.", Tuple$new(value)$strprint(), aid)
            }
          } else {
            if (!param$support[[1]]$contains(value, all = TRUE)) {
              stopf("%s does not lie in the support of '%s'.", value, aid)
            }
          }

          data.table::set(private$.parameters,
                          which(private$.parameters$id == aid),
                          "value",
                          list(value))

          private$.update(aid)
        }
      }

      x = try(mapply(private$.check, private$.parameters$id, private$.parameters$value),
              silent = TRUE)

      if (class(x) == "try-error") {
        private$.parameters <- orig
        stop(x)
      }

      invisible(self)
    },

    #' @description
    #' Merges multiple parameter sets.
    #' @param y `([ParameterSet])`
    #' @param ... `([ParameterSet]s)`
    #' @examples
    #' ps1 <- ParameterSet$new(id = c("prob", "qprob"),
    #'                  value = c(0.2, 0.8),
    #'                  support = list(set6::Interval$new(0, 1), set6::Interval$new(0, 1))
    #'  )
    #'  ps1$addChecks("prob", function(x, self) x > 0)
    #'  ps1$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
    #'  ps2 <- ParameterSet$new(id = "size",
    #'                  value = 10,
    #'                  support = set6::Interval$new(0, 10, class = "integer"),
    #'  )
    #'  ps2$addTrafos("size", function(x, self) x + 1)
    #'  ps1$merge(ps2)
    #'  ps1$print()
    #'  ps1$trafos
    #'  ps1$checks
    #'  ps1$deps
    merge = function(y, ...) {
      newsets <- c(list(y), list(...))
      lapply(newsets, function(x) {
        checkmate::assert(inherits(x, "ParameterSet"),
                          .var.name = "All objects in merge must be ParameterSets")
      })

      newpar <- rbind(
        as.data.table(self),
        data.table::rbindlist(lapply(newsets, function(x) as.data.table(x)))
      )


      if (any(table(newpar$id) > 1)) {
        stop("IDs must be unique. Try using makeUniqueDistributions first.")
      } else {
        private$.parameters <- newpar
        private$.trafos <- rbind(private$.trafos,
                                 data.table::rbindlist(lapply(newsets, function(x) x$trafos)))
        private$.checks <- rbind(private$.checks,
                                 data.table::rbindlist(lapply(newsets, function(x) x$checks)))
        private$.deps <- rbind(private$.deps,
                                 data.table::rbindlist(lapply(newsets, function(x) x$deps)))
      }
      invisible(self)
    },

    #' @description
    #' Add parameter dependencies for automatic updating.
    #' @param x `(character(1))`\cr
    #' id of parameter that updates `y`.
    #' @param y `(character(1))`\cr
    #' id of parameter that is updated by `x`.
    #' @param fun `(function(1))`\cr
    #' Function used to update `y`, must include `self` in formal arguments and
    #' `self$getParameterValue("<x>")` in body where `"<x>"` is the `id` supplied to `x`.
    #' See first example.
    #' @param dt `([data.table::data.table])`\cr
    #' Alternate method to directly construct `data.table` of dependencies to add.
    #' See second example.
    #' @examples
    #' id <- list("rate", "scale")
    #' value <- list(1, 1)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' settable <- list(TRUE, FALSE)
    #' ps <- ParameterSet$new(
    #'   id, value, support, settable
    #' )
    #' ps$addDeps("scale", "rate", function(self) 1 / self$getParameterValue("scale"))
    #' ps$addDeps("rate", "scale", function(self) 1 / self$getParameterValue("rate"))
    #' ps$deps
    #'
    #' # Alternate method
    #' ps <- ParameterSet$new(
    #'   id, value, support, settable
    #' )
    #' ps$addDeps(dt = data.table::data.table(x = c("scale", "rate"),
    #'                           y = c("rate", "scale"),
    #'                           fun = c(function(self) 1 / self$getParameterValue("scale"),
    #'                                   function(self) 1 / self$getParameterValue("rate"))
    #'                          )
    #'            )
    #' ps$deps
    addDeps = function(x, y, fun, dt = NULL) {
      if (is.null(dt)) {
        dt <- data.table(x = x, y = y, fun = fun)
      }

      checkmate::assertDataTable(dt, types = c("character", "character", "list"))
      checkmate::assertNames(colnames(dt), identical.to = c("x", "y", "fun"))

      apply(dt, 1, function(z) {
        if(nrow(subset(private$.parameters, id == z[[1]])) == 0) {
          stopf("%s is not a parameter in this ParameterSet", z[[1]])
        }
        if(nrow(subset(private$.parameters, id == z[[2]])) == 0) {
          stopf("%s is not a parameter in this ParameterSet", z[[2]])
        }
        checkmate::assertFunction(z[[3]], "self")
      })

      private$.deps <- rbind(self$deps, dt)

      invisible(self)
    },

    #' @description
    #' Add parameter checks for automatic assertions. Note checks are made after
    #' any transformations.
    #' @param x `(character(1))`\cr
    #' id of parameter to be checked.
    #' @param fun `(function(1))`\cr
    #' Function used to check `x`, must include `x, self` in formal arguments and
    #' `x` in body where `x` is the value of the parameter to check. Result should
    #' be a logical. See first example.
    #' @param dt `([data.table::data.table])`\cr
    #' Alternate method to directly construct `data.table` of checks to add.
    #' See second example.
    #' @examples
    #' id <- list("lower", "upper")
    #' value <- list(1, 3)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' ps <- ParameterSet$new(
    #'   id, value, support
    #' )
    #' ps$addChecks("lower", function(x, self) x < self$getParameterValue("upper"))
    #' ps$checks
    #' \dontrun{
    #' # errors as check fails
    #' ps$setParameterValue(lower = 4)
    #' }
    #' ps$setParameterValue(lower = 2)
    #'
    #' # Alternate method (better with more parameters)
    #' ps <- ParameterSet$new(
    #'   id, value, support
    #' )
    #' ps$addChecks(dt = data.table::data.table(
    #'                           x = "lower",
    #'                           fun = function(x, self) x < self$getParameterValue("upper")
    #'            ))
    addChecks = function(x, fun, dt = NULL) {
      if (is.null(dt)) {
        dt <- data.table(x = x, fun = fun)
      }

      checkmate::assertDataTable(dt, types = c("character", "list"))
      checkmate::assertNames(colnames(dt), identical.to = c("x", "fun"))

      apply(dt, 1, function(z) {
        if (nrow(subset(private$.parameters, id == z[[1]])) == 0) {
          stopf("%s is not a parameter in this ParameterSet", z[[1]])
        }
        checkmate::assertFunction(z[[2]], c("x", "self"), TRUE)
      })

      private$.checks <- rbind(self$checks, dt)

      invisible(self)
    },

    #' @description
    #' Transformations to apply to parameter before setting. Note transformations are made before
    #' checks.
    #' @param x `(character(1))`\cr
    #' id of parameter to be transformed. Only one trafo function per parameter allowed - though
    #' multiple transformations can be encoded within this.
    #' @param fun `(function(1))`\cr
    #' Function used to transform `x`, must include `x, self` in formal arguments and
    #' `x` in body where `x` is the value of the parameter to check.  See first example.
    #' @param dt `([data.table::data.table])`\cr
    #' Alternate method to directly construct `data.table` of transformations to add.
    #' See second example.
    #' @examples
    #' ps <- ParameterSet$new(
    #'   "probs", list(c(1, 1)), set6::Interval$new(0,1)^2
    #' )
    #' ps$addTrafos("probs", function(x, self) return(x / sum(x)))
    #' ps$trafos
    #' ps$setParameterValue(probs = c(1, 2))
    #' ps$getParameterValue("probs")
    #'
    #' # Alternate method (better with more parameters)
    #' ps <- ParameterSet$new(
    #'   "probs", list(c(1, 1)), set6::Interval$new(0,1)^2
    #' )
    #' ps$addTrafos(dt = data.table::data.table(
    #'                           x = "probs",
    #'                           fun = function(x, self) return(x / sum(x))
    #'            ))
    addTrafos = function(x, fun, dt = NULL) {
      if (is.null(dt)) {
        dt <- data.table(x = x, fun = fun)
      }

      checkmate::assertDataTable(dt, types = c("character", "list"))
      checkmate::assertNames(colnames(dt), identical.to = c("x", "fun"))

      apply(dt, 1, function(z) {
        if (nrow(subset(private$.parameters, id == z[[1]])) == 0) {
          stopf("%s is not a parameter in this ParameterSet", z[[1]])
        }
        if (nrow(subset(self$trafos, x == z[[1]])) > 0) {
          warning("%s already has a `trafo` function, this will be overwritten.")
          ans <- readline("Proceed? Y/N")
          if(ans == "Y") {
            private$.trafos <- subset(self$trafos, x != z[[1]])
          } else {
            invisible(self)
          }
        }
        checkmate::assertFunction(z[[2]], c("x", "self"), TRUE)
      })

      private$.trafos <- rbind(self$trafos, dt)

      invisible(self)
    }
  ),

  active = list(
    #' @field values
    #' Returns parameter set values as a named list.
    values = function() {
      pars <- subset(private$.parameters, settable == TRUE)
      values <- pars$value
      names(values) <- pars$id
      return(values)
    },

    #' @field deps
    #' Returns ParameterSet dependencies table.
    deps = function() {
      return(private$.deps)
    },

    #' @field checks
    #' Returns ParameterSet assertions table.
    checks = function() {
      return(private$.checks)
    },

    #' @field trafos
    #' Returns ParameterSet transformations table.
    trafos = function() {
      return(private$.trafos)
    }
  ),

  private = list(
    .parameters = data.table(id = character(), value = list(), support = list(),
                             settable = logical(), description = character()),
    .deps = data.table(x = character(), y = character(), fun = list()),
    .checks = data.table(x = character(), fun = list()),
    .trafos = data.table(x = character(), fun = list()),
    .setParameterSupport = function(lst) {
      id <- names(lst)
      support <- lst[[1]]
      which <- which(unlist(private$.parameters$id) %in% id)
      private$.parameters[which, 3][[1]] <- list(support)
      invisible(self)
    },
    .check = function(id, value) {
      checks = subset(self$checks, x == id)
      if (nrow(checks)) {
        for (i in seq(nrow(checks))) {
          assert(checks$fun[[i]](value, self))
        }
      }
      invisible(self)
    },
    .trafo = function(id, value) {
      trafos = subset(self$trafos, x == id)
      if (nrow(trafos)) {
        return(trafos$fun[[1]](value, self))
      } else {
        return(value)
      }
    },
    .update = function(id) {
      updates = subset(self$deps, x == id)
      if (nrow(updates)) {
        for(i in seq(nrow(updates))) {
          data.table::set(private$.parameters,
                          which(private$.parameters$id == updates$y[[i]]),
                          "value",
                          list(updates$fun[[i]](self)))
        }
      }

      invisible(self)
    },
    deep_clone = function(name, value) {
      if (name %in% c(".parameters", ".deps", ".checks")) {
        data.table::copy(value)
      } else {
        value
      }
    }
  )
)

#' @export
c.ParameterSet <- function(..., prefix.names = NULL) {
  if (!is.null(prefix.names)) {
    stopifnot(length(prefix.names) == ...length())
  }

  ps <- data.table(
    id = NULL, value = NULL, support = NULL, settable = NULL,
    description = NULL
  )

  for (i in seq(...length())) {
    dt <- as.data.table(...elt(i))
    if (!is.null(prefix.names)) {
      dt$id <- paste(prefix.names[[i]], dt$id, sep = "_")
    }
    ps <- rbind(ps, dt)
  }

  return(as.ParameterSet(ps))
}

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
#' for details. Similarly for lists, names are taken to be ParameterSet parameters and values taken
#' to be arguments.
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
    description = x$description
  ))
}

#' @rdname as.ParameterSet
#' @export
as.ParameterSet.list <- function(x, ...) {
  return(ParameterSet$new(
    id = x$id, value = x$value, support = x$support,
    settable = x$settable,
    description = x$description
  ))
}


#' @title Extract one or more parameters from a ParameterSet
#' @description Used to extract one or more parameters from a constructed [ParameterSet] or
#' [ParameterSetCollection].
#'
#' @param ps [ParameterSet]\cr
#' [ParameterSet] from which to extract parameters.
#' @param ids `(character())` \cr
#' ids of parameters to extract, if `id` ends with `_` then all parameters starting
#' with `ids_` are extracted and the prefix is ignored, `prefix` can be left `NULL`.
#' See examples.
#' @param prefix `(character(1))` \cr
#' An optional prefix to remove from ids after extraction, assumes `_` follows the
#' prefix name, i.e. `prefix_ids`.
#' @param ... \cr
#' Ignored, added for consistency.
#'
#' @usage \method{[}{ParameterSet}(ps, ids, prefix = NULL, ...)
#'
#' @examples
#' ps <- VectorDistribution$new(
#'   distribution = "Binomial",
#'   params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))
#' )$parameters()
#'
#' ps["Binom1_prob"] # extracts just Binom1_prob
#' ps["Binom1_prob", prefix = "Binom1"] # extracts Binom1_prob and removes prefix
#' ps["Binom1_"] # extracts all Binom1 parameters and removes prefix
#' @export
"[.ParameterSet" <- function(ps, ids, prefix = NULL, ...) {
  id <- NULL # added to remove the NOTE, is overwritten immediately
  dt <- as.data.table(ps)
  if (grepl("_$", ids)) {
    dt <- subset(dt, grepl(ids, id))
    dt$id <- gsub(ids, "", dt$id)
  } else {
    dt <- subset(dt, id %in% ids)
    if (!is.null(prefix)) {
      dt$id <- gsub(paste0(prefix, "_"), "", dt$id)
    }
  }

  return(as.ParameterSet(dt))
}
