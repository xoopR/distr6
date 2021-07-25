`%nin%` <- function(x, table) {
  !(x %in% table)
}

assertThat <- function(x, cond, errormsg) {
  if (cond) {
    invisible(x)
  } else {
    stop(errormsg)
  }
}
checkThat <- function(cond, errormsg) {
  if (cond) {
    return(TRUE)
  } else {
    return(errormsg)
  }
}
testThat <- function(cond) {
  if (cond) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
isThat <- function(cond) {
  return(testThat(cond))
}

makeChecks <- function(assertionName, cond, defaulterrormsg, args = alist(object = , errormsg = ), # nolint
                       pos = -1) {
  cond <- substitute(cond)
  value <- function() {}
  args$errormsg <- substitute(defaulterrormsg)
  formals(value) <- args
  body(value) <- substitute(assertThat(object, arg1, errormsg), list(arg1 = cond))
  assign(paste0("assert", assertionName),
    value = value,
    pos = pos
  )

  body(value) <- substitute(checkThat(arg1, errormsg), list(arg1 = cond))
  assign(paste0("check", assertionName),
    value = value,
    pos = pos
  )

  body(value) <- substitute(testThat(arg1), list(arg1 = cond))
  assign(paste0("test", assertionName),
    value = value,
    pos = pos
  )
}

getR6Class <- function(object, classname = TRUE, n.par = 0, pos = -1) {
  if (R6::is.R6(object)) {
    if (classname) {
      return(get(class(object)[[n.par + 1]], pos = pos)$classname)
    } else {
      return(get(class(object)[[n.par + 1]], pos = pos))
    }
  } else {
    return(class(object))
  }
}
ifnerror <- function(expr, noerror, error = NULL) {

  x <- try(expr, silent = TRUE)
  if (inherits(x, "try-error")) {
    if (is.null(error) | error == "warn") {
      stopwarn("warn", "Error not Nerror!")
    } else if (error == "stop") {
      stopwarn("stop", "Error not Nerror!")
    } else if (error == "NULL") {
      return(NULL)
    } else {
      error
    }
  } else {
    if (missing(noerror)) {
      return(x)
    } else {
      return(noerror)
    }
  }
}

stopwarn <- function(error = "warn", error.msg) {
  checkmate::assert(error == "warn", error == "stop",
    .var.name = "'error' should be one of 'warn' or 'stop'."
  )
  if (error == "stop") {
    stop(error.msg)
  } else {
    warning(error.msg, call. = F)
    return(NULL)
  }
}
testMessage <- function(expr) {
  if (inherits(tryCatch(expr, message = function(m) m), "message")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

modal <- function(data) {
  tab <- table(unlist(data))
  modal <- as.numeric(names(tab)[tab == max(tab)])
  return(modal)
}

makeUniqueNames <- function(y) {
  if (any(duplicated(y))) {
    count <- table(y)
    for (i in seq_along(count)) {
      if (count[i] > 1) {
        y[y == names(count[i])] <- paste0(names(count[i]), 1:count[i])
      }
    }
  }

  return(y)
}

toproper <- function(str, split = " ", fixed = TRUE) {
  str <- strsplit(str, split, fixed)
  str <- lapply(str, function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 1000)), collapse = split)
  })
  return(unlist(str))
}

assert_pkgload <- function(pkgs) {
  if (!is.null(pkgs)) {
    check <- sapply(pkgs, requireNamespace, quietly = TRUE)
    if (!all(check)) {
      stop(sprintf(
        "The following packages could not be loaded, please install: %s",
        paste0("{", paste0(pkgs[!check], collapse = ","), "}")
      ))
    }
  }
}

assertOneWord <- function(x, errormsg = "All values must be one word.") {
  check <- grepl(" ", x, fixed = TRUE)
  if (any(check)) {
    stop(errormsg)
  } else {
    invisible(x)
  }
}

strCollapse <- function(x, par = "{}") {
  paste0(substr(par, 1, 1), paste0(x, collapse = ", "), substr(par, 2, 2))
}

test_list <- function(x) {
  class(x)[1] == "list"
}

impute_genx <- function(dist, n = 10001) {

  x <- dist$workingSupport()
  if (testDiscrete(dist)) {
    x <- seq.int(x$lower, x$upper)
  } else {
    if (n %% 2 == 0) {
      n <- n + 1
    }

    x <- seq.int(x$lower, x$upper, length.out = n)
  }

  return(x)
}

rlapply <- function(X, FUN, ..., active = FALSE) {
  FUN <- as.character(substitute(FUN))
  if (active) {
    return(lapply(X, function(x) x[[FUN]]))
  } else {
    return(lapply(X, function(x) x[[FUN]](...)))
  }
}
rsapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, active = FALSE) {
  FUN <- as.character(substitute(FUN))
  if (active) {
    return(sapply(X, function(x) x[[FUN]], simplify = simplify, USE.NAMES = USE.NAMES))
  } else {
    return(sapply(X, function(x) x[[FUN]](...), simplify = simplify, USE.NAMES = USE.NAMES))
  }
}

abstract <- function(obj, class, see) {
  if (getR6Class(obj) == class) {
    if (missing(see)) {
      stopf("%s is an abstract class that can't be initialized.", class)
    } else {
      stopf("%s is an abstract class that can't be initialized. Instead see %s.", class, see)
    }
  }
}

stopf <- function(str, ...) {
  stop(sprintf(str, ...))
}

v_genfun <- function(x, fun) {
  if (length(x) == 1) {
    return(fun(x))
  } else {
    return(sapply(x, fun))
  }
}

getR6Call <- function() {
  # get call
  calls <- as.list(match.call(definition = sys.function(sys.parent(2L)),
                 call = sys.call(sys.parent(3L)),
                 envir = parent.frame(4L)))[-1]
  calls <- calls[names(calls) %nin% "decorators"]
  # prevent lazy evaluation
  lapply(calls, eval.parent, n = 5)
}


unique_nlist <- function(x) {
  x[!duplicated(names(x))]
}


expand_list <- function(names, named_var) {
  checkmate::assert_character(names)
  checkmate::assert_list(named_var)

  mtc <- match(names(named_var), names)
  if (any(is.na(mtc))) {
    stop("ids in 'names' not in 'named_var'")
  }

  x <- setNames(vector("list", length(names)), names)
  x[mtc] <- named_var
  x
}

list_element <- function(x, name) {
  x[grepl(sprintf("(__%s$)|(^%s$)", name, name), names(x))]
}

named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    setNames(list(), character())
  } else {
    setNames(list(values), names)
  }
}

as_named_list <- function(values, names) {
  if (missing(values) && missing(names)) {
    setNames(list(), character())
  } else {
    setNames(as.list(values), names)
  }
}


get_private <- function(x) {
  x$.__enclos_env__$private
}


sort_named_list <- function(x, ...) {
  x[order(names(x), ...)]
}


unprefix <- function(x) {
  gsub("([[:alnum:]]+)__(\\S*)", "\\2", x)
}

get_prefix <- function(x) {
  gsub("([[:alnum:]]+)__(\\S*)", "\\1", x)
}

get_n_prefix <- function(x) {
  gsub("(\\S+)__(\\S*)", "\\1", x)
}


assert_alphanum <- function(x) {
  if (any(grepl("[^[:alnum:]]", x))) {
    stop("'x' must be alphanumeric")
  }
  invisible(x)
}


drop_null <- function(x) {
  x[vapply(x, function(.x) length(.x) > 0, logical(1))]
}
