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
    #' @param updateFunc `(list())` \cr
    #' Deprecated, please use `$addDeps` instead.
    #' @param description `(character(1)|list())`\cr
    #' Optional description for the parameter(s).
    initialize = function(id, value, support, settable = TRUE,
                          updateFunc = NULL,
                          description = NULL) {

      if (missing(id)) {
        return(invisible(self))
      }

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
      checkmate::assert(length(unique(
        length(id), length(value), length(settable),
        length(support)
      )) == 1,
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
          assertContains(y, Tuple$new(x),
                         paste(strCollapse(x, "()"), "does not lie in the set", y$strprint()))
        } else {
          if (inherits(y, "ExponentSet")) {
            if (y$power == "n") {
              assertContains(y, Tuple$new(x),
                             paste(strCollapse(x, "()"), "does not lie in the set", y$strprint()))
            } else {
              assertContains(y, x, paste(x, "does not lie in the set", y$strprint()))
            }
          } else {
            assertContains(y, x, paste(x, "does not lie in the set", y$strprint()))
          }
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
    #' @param ... `ANY` \cr
    #' Additional arguments, currently unused.
    print = function(hide_cols = c("settable"), ...) {
      ps <- private$.parameters
      ps$support <- lapply(ps$support, function(x) x$strprint())
      print(subset(ps, select = names(ps) %nin% hide_cols))
    },

    #' @description
    #' Returns the full parameter details for the supplied parameter, or returns `self`
    #' if `id` is `NULL`.
    #' @param id `character()` \cr
    #' id of parameter to return.
    parameters = function(id = NULL) {
      if (!is.null(id)) {
        id0 <- id
        pars <- subset(private$.parameters, id %in% id0)
        if (nrow(pars) == 0) {
          stopf("'%s' is not a parameter in this ParameterSet.", id)
        } else {
          return(pars)
        }
      } else {
        return(self)
      }
    }

    #' @description
    #' Returns the value of the supplied parameter.
    getParameterValue = function(id, error = "warn") {
      if (missing(id)) {
        return(stopwarn(error, "Argument 'id' is missing, with no default."))
      }

      self$parameters(id)[["value"]][[1]]
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    #' @param .suppressCheck `(logical(1))`\cr
    #' Should be set internally only.
    setParameterValue = function(..., lst = NULL, error = "warn", .suppressCheck = FALSE,
                                 resolveConflicts = FALSE) {
      if (is.null(lst)) {
        lst <- list(...)
      } else {
        checkmate::assertList(lst)
      }

      lst <- lst[!sapply(lst, is.null)]

      if (length(private$.deps)) {
        if (resolveConflicts) {
          conflicts <- lapply(names(lst), function(.x) {
            check <- self$deps$x %in% .x
            if (any(check)) {
              if (any(names(lst) %in% self$deps$y[[which(check)]])) {
                return(names(lst)[names(lst) %in% self$deps$y[[which(check)]]])
              }
            }
          })
          names(conflicts) <- names(lst)
          # FIXME - In future this will be resolved in param6 with tags
          #  Temp hacky fix to remove conflicts by iteratively deleting params that are in
          #  both x and y of deps
          for (i in seq.int(length(conflicts), 1, -1)) {
            if (names(conflicts)[i] %in% unlist(conflicts)) {
              conflicts <- conflicts[i]
            }
          }
        } else {

          lapply(names(lst), function(.x) {
            check <- self$deps$x %in% .x
            if (any(check)) {
              if (any(names(lst) %in% self$deps$y[[which(check)]])) {
                if (resolveConflicts) {
                  return(names(lst)[names(lst) %in% self$deps$y[[which(check)]]])
                } else {
                  stop(sprintf("Conflicting parametrisations detected. Only one of %s should be given.", # nolint
                               strCollapse(c(.x, self$deps$y[[which(check)]]))))
                }
              }
            }
          })
        }
      }

      orig <- data.table::copy(private$.parameters) # FIXME - Hacky fix for checks

      checkmate::assertSubset(names(lst), as.character(unlist(orig$id)))
      dt <- subset(private$.parameters, id %in% names(lst))

      if (!all(dt$settable)) {
        warning(sprintf("%s is not settable after construction, parameter(s) ignored.",
                     strCollapse(unlist(subset(dt, !settable)$id))))
        dt <- subset(dt, settable)
      }

      if (nrow(dt)) {
        dt$value <- lst[match(dt$id, names(lst))]
        dt[, value := Map(private$.trafo, id = id, value = value)]

        if (!.suppressCheck) {
          apply(dt, 1, function(.x) {
            value <- .x[[2]]
            support <- .x[[3]]
            if (length(value) > 1 || (inherits(support, "ExponentSet") && support$power == "n")) {
              value <- Tuple$new(value)
            }
            assertContains(support, value,
                           sprintf("%s does not lie in the support of %s.",
                                   strCollapse(value, "()"), .x[[1]]))
          })
        }

        data.table::set(
          private$.parameters,
          which(private$.parameters$id %in% dt$id),
          "value",
          dt$value
        )

        sapply(unlist(dt$id), private$.update)

        if (!is.null(self$checks) && !.suppressCheck) {
          x <- try(private$.check(), silent = TRUE)
          if (class(x) == "try-error") {
            private$.parameters <- orig
            stop("ParameterSet checks failed.")
          }
        }
      }

      invisible(self)
    },

    #' @description
    #' Merges multiple parameter sets.
    #' @param y `([ParameterSet])`
    #' @param ... `([ParameterSet]s)`
    merge = function(y, ...) {
      newsets <- c(list(y), list(...))
      lapply(newsets, function(x) {
        checkmate::assert(inherits(x, "ParameterSet"),
          .var.name = "All objects in merge must be ParameterSets"
        )
      })

      newpar <- rbind(
        as.data.table(self),
        data.table::rbindlist(lapply(newsets, function(x) as.data.table(x)))
      )


      if (any(table(newpar$id) > 1)) {
        stop("IDs must be unique. Try using makeUniqueDistributions first.")
      } else {
        private$.parameters <- newpar
        private$.trafos <- rbind(
          private$.trafos,
          data.table::rbindlist(lapply(newsets, function(x) x$trafos))
        )
        private$.checks <- c(
          private$.checks,
          lapply(newsets, function(x) x$checks)
        )
        private$.deps <- rbind(
          private$.deps,
          data.table::rbindlist(lapply(newsets, function(x) x$deps))
        )
      }
      invisible(self)
    },

    #' @description
    #' Add parameter dependencies for automatic updating.
    #' @param x `(character(1))`\cr
    #' id of parameter that updates `y`.
    #' @param y `(character())`\cr
    #' id of parameter(s) that is/are updated by `x`.
    #' @param fun `(function(1))`\cr
    #' Function used to update `y`, must include `self` in formal arguments and should return a
    #' named list with names identical to, and in the same order, as `y`.
    addDeps = function(x, y, fun) {
      checkmate::assertFunction(fun, "self")
      checkmate::assertSubset(c(x, y), unlist(private$.parameters$id))
      private$.deps <- rbind(self$deps, data.table(x = x, y = list(y), fun = fun))
      invisible(self)
    },

    #' @description
    #' Add parameter checks for automatic assertions. Note checks are made after
    #' any transformations.
    #' @param fun `(function(1))`\cr
    #' Function used to check `ParameterSet`, must include `self` in formal arguments and
    #' result in a logical.
    addChecks = function(fun) {
      if (is.null(self$checks)) {
        private$.checks <- checkmate::assertFunction(fun, "self")
      } else {
        f1 <- self$checks
        f2 <- checkmate::assertFunction(fun)
        body(f1) <- substitute(b1 && b2, list(b1 = body(f1), b2 = body(f2)))
        private$.checks <- f1
      }

      invisible(self)
    },

    #' @description
    #' Transformations to apply to parameter before setting. Note transformations are made before
    #' checks.
    #' NOTE: If a transformation for a parameter already exists then this will be overwritten.
    #' @param x `(character(1))`\cr
    #' id of parameter to be transformed. Only one trafo function per parameter allowed - though
    #' multiple transformations can be encoded within this.
    #' @param fun `(function(1))`\cr
    #' Function used to transform `x`, must include `x, self` in formal arguments and
    #' `x` in body where `x` is the value of the parameter to check.  See first example.
    #' @param dt `([data.table::data.table])`\cr
    #' Alternate method to directly construct `data.table` of transformations to add.
    #' See second example.
    addTrafos = function(x, fun, dt = NULL) {
      if (is.null(dt)) {
        dt <- data.table(x = x, fun = fun)
      }

      checkmate::assertDataTable(dt, types = c("character", "list"))
      checkmate::assertNames(colnames(dt), identical.to = c("x", "fun"))

      apply(dt, 1, function(z) {
        if (nrow(subset(private$.parameters, id == z[[1]])) == 0) {
          stopf("'%s' is not a parameter in this ParameterSet", z[[1]])
        }
        if (nrow(subset(self$trafos, x == z[[1]])) > 0) {
          warning(sprintf("'%s' already has a `trafo` function, this will be overwritten.", z[[1]]))
          private$.trafos <- subset(self$trafos, x != z[[1]])
        }
        checkmate::assertFunction(z[[2]], c("x", "self"), TRUE)
      })

      private$.trafos <- rbind(self$trafos, dt)

      invisible(self)
    },

    #' @description
    #' Returns parameter set values as a named list.
    #' @param settable `(logical(1))`\cr
    #' If `TRUE` (default) only returns values of settable parameters.
    values = function(settable = TRUE) {
      if (settable) {
        pars <- subset(private$.parameters, settable == TRUE)
      } else {
        pars <- private$.parameters
      }

      values <- pars$value
      names(values) <- pars$id
      return(values)
    }
  ),

  active = list(
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
    },

    #' @field length
    #' Number of parameters in ParameterSet.
    length = function() {
      nrow(private$.parameters)
    }
  ),

  private = list(
    .parameters = data.table(
      id = character(), value = list(), support = list(),
      settable = logical(), description = character()
    ),
    .deps = data.table(x = character(), y = character(), fun = list()),
    .checks = NULL,
    .trafos = data.table(x = character(), fun = list()),
    .setParameterSupport = function(lst) {
      id <- names(lst)
      support <- lst[[1]]
      which <- which(unlist(private$.parameters$id) %in% id)
      private$.parameters[which, 3][[1]] <- list(support)
    },
    .check = function() {
      assert(self$checks(self))
    },
    .trafo = function(id, value) {
      trafos <- subset(self$trafos, x == id)
      if (nrow(trafos)) {
        return(trafos$fun[[1]](value, self))
      } else {
        return(value)
      }
    },
    .update = function(id) {
      updates <- subset(self$deps, x == id)
      if (nrow(updates)) {
        # FIXME - need to sort in alphabetical order, this can definitely be improved
        rows <- unlist(private$.parameters$id) %in% unlist(updates$y)
        rows <- which(rows)[order(unlist(private$.parameters$id)[rows])]
        update <- updates$fun[[1]](self)
        update <- update[order(names(update))]
        data.table::set(
          private$.parameters,
          rows,
          "value",
          update
        )
      }
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
      dt$id <- paste(prefix.names[[i]], dt$id, sep = "__")
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
#' @usage setParameterValue(object, ..., lst = NULL, error = "warn", resolveConflicts = FALSE)
#'
#' @param object Distribution or ParameterSet.
#' @param ... named parameters and values to update, see details.
#' @param lst optional list, see details.
#' @param error character, value to pass to \code{stopwarn}.
#' @param resolveConflicts `(logical(1))`\cr
#' If `FALSE` (default) throws error if conflicting parameterisations are provided, otherwise
#' automatically resolves them by removing all conflicting parameters.
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



#' @title Extract one or more parameters from a ParameterSet
#' @description Used to extract one or more parameters from a constructed [ParameterSet] or
#' [ParameterSetCollection].
#'
#' @param ps [ParameterSet]\cr
#' [ParameterSet] from which to extract parameters.
#' @param ids `(character())` \cr
#' ids of parameters to extract, if `id` ends with `__` then all parameters starting
#' with `ids__` are extracted and the prefix is ignored, `prefix` can be left `NULL`.
#' See examples.
#' @param prefix `(character(1))` \cr
#' An optional prefix to remove from ids after extraction, assumes `__` follows the
#' prefix name, i.e. `prefix__ids`.
#' @param ... `ANY` \cr
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
#' ps["Binom1__prob"] # extracts just Binom1__prob
#' ps["Binom1__prob", prefix = "Binom1"] # extracts Binom1__prob and removes prefix
#' ps["Binom1__"] # extracts all Binom1 parameters and removes prefix
#' @export
"[.ParameterSet" <- function(ps, ids, prefix = NULL, ...) {
  id <- NULL # added to remove the NOTE, is overwritten immediately
  dt <- as.data.table(ps)
  if (grepl("__$", ids)) {
    dt <- subset(dt, grepl(paste0("^", ids), id))
    dt$id <- gsub(ids, "", dt$id)
  } else {
    dt <- subset(dt, id %in% ids)
    if (!is.null(prefix)) {
      dt$id <- gsub(paste0(prefix, "__"), "", dt$id, fixed = TRUE)
    }
  }

  return(as.ParameterSet(dt))
}
