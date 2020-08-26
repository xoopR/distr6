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
        pars <- subset(private$.parameters, id %in% id0)
        if (nrow(pars) == 0) {
          stopf("'%s' is not a parameter in this ParameterSet.", id)
        } else {
          return(pars)
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
    #' @param .suppressCheck `(logical(1))`\cr
    #' Should be set internally only.
    #'
    #' @examples
    #' id <- list("rate")
    #' value <- list(1)
    #' support <- list(set6::PosReals$new())
    #' ps <- ParameterSet$new(
    #'   id, value, support
    #' )
    #' ps$setParameterValue(rate = 2)
    #' ps$getParameterValue("rate")
    setParameterValue = function(..., lst = NULL, error = "warn", .suppressCheck = FALSE) {
      if (is.null(lst)) {
        lst <- list(...)
      } else {
        checkmate::assertList(lst)
      }

      lst <- lst[!sapply(lst, is.null)]

      orig <- data.table::copy(private$.parameters) # FIXME - Hacky fix for checks

      checkmate::assertSubset(names(lst), as.character(unlist(orig$id)))
      dt <- subset(private$.parameters, id %in% names(lst))

      if (!all(dt$settable)) {
        stop(sprintf("%s is not settable after construction.",
                     strCollapse(unlist(subset(dt, !settable)$id))))
      }

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
          stop(x)
        }
      }

      invisible(self)
    },

    #' @description
    #' Merges multiple parameter sets.
    #' @param y `([ParameterSet])`
    #' @param ... `([ParameterSet]s)`
    #' @examples
    #' \dontrun{
    #' ps1 <- ParameterSet$new(id = c("prob", "qprob"),
    #'                  value = c(0.2, 0.8),
    #'                  support = list(set6::Interval$new(0, 1), set6::Interval$new(0, 1))
    #'  )
    #'  ps1$addChecks(function(self) self$getParameterValue("x") > 0)
    #'  ps1$addDeps("prob", "qprob", function(self)
    #'      list(qprob = 1 - self$getParameterValue("prob")))
    #'  ps2 <- ParameterSet$new(id = "size",
    #'                  value = 10,
    #'                  support = set6::Interval$new(0, 10, class = "integer"),
    #'  )
    #'  ps2$addTrafos("size", function(x, self) x + 1)
    #'  ps1$merge(ps2)
    #'  ps1$print()
    #'  }
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
    #' @examples
    #' \dontrun{
    #' ps <- ParameterSet$new(
    #'   id = list("a", "b", "c"),
    #'   value = list(2, 3, 1/2),
    #'   support = list(set6::Reals$new(), set6::Reals$new(), set6::Reals$new())
    #' )
    #' ps$addDeps("a", c("b", "c"),
    #'    function(self) {
    #'        list(b = self$getParameterValue("a") + 1,
    #'             c = 1/self$getParameterValue("a"))
    #'  })
    #' }
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
    #' @examples
    #' \dontrun{
    #' id <- list("lower", "upper")
    #' value <- list(1, 3)
    #' support <- list(set6::PosReals$new(), set6::PosReals$new())
    #' ps <- ParameterSet$new(
    #'   id, value, support
    #' )
    #' ps$addChecks(function(self)
    #'   self$getParameterValue("lower") < self$getParameterValue("upper"))
    #' }
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
    #' @examples
    #' \dontrun{
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
    #' }
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
    #' If `TRUE` (default) only returns values of settable parameters, otherwise returns all.
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
