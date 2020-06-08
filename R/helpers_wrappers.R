mixture_dpqr_returner <- function(dpqr, weights, univariate) {
  if (univariate) {
    if (checkmate::testNumeric(weights)) {
      return(as.numeric(weights %*% t(as.matrix(dpqr))))
    } else {
      if (nrow(dpqr) == 1) {
        return(as.numeric(sum(dpqr) / length(dpqr)))
      } else {
        return(as.numeric(apply(dpqr, 1, sum) / length(dpqr)))
      }
    }
  } else {
    if (checkmate::testNumeric(weights)) {
      ret <- apply(dpqr, 2, function(x) weights %*% t(as.matrix(x)))
    } else {
      ret <- apply(dpqr, 2, rowMeans)
    }
    if (inherits(ret, "matrix")) {
      return(data.table(ret))
    } else {
      ret <- data.table::transpose(data.table(ret))
      colnames(ret) <- colnames(dpqr)
      return(ret)
    }
  }
}

product_dpqr_returner <- function(dpqr, univariate) {
  if (univariate) {
    if (nrow(dpqr) == 1) {
      return(prod(dpqr))
    } else {
      return(as.numeric(apply(dpqr, 1, prod)))
    }
  } else {
    if (dim(dpqr)[1] == 1) {
      return(apply(dpqr, 2, prod))
    } else {
      return(apply(dpqr, 2, function(x) apply(x, 1, prod)))
    }
  }
}
