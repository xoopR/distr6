pdq_point_assert <- function(..., self, data) {
  if (is.null(data)) {
    if (...length() == 0) {
      stop("Points to evaluate must be passed to `...` or `data`.")
    } else {
      if (testUnivariate(self)) {
        if (...length() > 1) {
          warning("Distribution is univariate, only values passed to the first argument are kept.")
        }
        data <- ...elt(1)
      } else if (testMultivariate(self)) {
        if (...length() < 2) {
          stop("Distribution is multivariate but values have only been passed to one argument.")
        } else {
          data <- as.matrix(data.table(...))
        }
      }
      # TODO - Once matrixvariates implemented
      # } else {
      #   stop("Points to evaluate must be passed to `data` for matrixvariate distributions.")
      # }
    }
  } else {
    if (testUnivariate(self)) {
      if (ncol(data) > 1) {
        warning("Distribution is univariate, only values in the first column are kept.")
      }
      data <- data[, 1]
    } else if (testMultivariate(self)) {
      if (ncol(data) == 1) {
        stop("Distribution is multivariate but values have only been passed to one argument.")
      } else {
        data <- as.matrix(data)
      }
    }
    # TODO - Once matrixvariates implemented
    # else {
    #   if (class(data) != "array") {
    #     stop("For matrixvariate distributions `data` must be an array.")
    #   }
    # }
  }

  return(data)
}
pdqr_returner <- function(pdqr, simplify, name) {
  if (inherits(pdqr, "data.table")) {
    return(pdqr)
  } else {
    if (simplify) {
      return(pdqr)
    } else {
      pdqr <- data.table(pdqr)
      if (ncol(pdqr) == 1) {
        colnames(pdqr) <- name
        return(pdqr)
      } else {
        colnames(pdqr) <- paste0(name, ".V", seq_along(pdqr))
        return(pdqr)
      }
    }
  }
}
call_C_base_pdqr <- function(fun, x, args, lower.tail = TRUE, log = FALSE, vec) {
  type <- substr(fun, 1, 1)

  if (!(type %in% c("d", "p", "q", "r"))) {
    stop("Function must start with one of: {d, p, q, r}.")
  }

  if (vec) {
    if (type == "r") {
      return(C_r(fun, x, args))
    } else {
      nr <- nrow(x)
      if (is.null(nr)) {
        bool <- TRUE
      } else {
        bool <- nrow(x) > 1
      }
      if (bool) {
        return(C_dpq(
          fun = fun,
          x = x,
          args = args,
          lower = lower.tail,
          log = log
        ))
      }
    }
  }

  if (type == "d") {
    return(do.call(get(fun), c(list(x, log = log), args)))
  } else if (type == "p") {
    return(do.call(get(fun), c(list(x, lower.tail = lower.tail, log.p = log), args)))
  } else if (type == "q") {
    return(do.call(get(fun), c(list(x, lower.tail = lower.tail, log.p = log), args)))
  } else if (type == "r") {
    return(do.call(get(fun), c(list(x), args)))
  }
}
