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
ifnerror <- function(expr, noerror, error = NULL, silent = T) {
  x <- try(expr, silent)
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
