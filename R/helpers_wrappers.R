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
      return(apply(dpqr, 1, function(x) weights %*% as.matrix(x)))
    } else {
      return(rowMeans(dpqr))
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
    return(apply(dpqr, 1, prod))
  }
}
