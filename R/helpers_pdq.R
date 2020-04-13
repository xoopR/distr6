pdq_point_assert = function(..., self, data){
  if(missing(data)){
    if(...length() == 0){
      stop("Points to evaluate must be passed to `...` or `data`.")
    } else {
      if (testUnivariate(self)) {
        if(...length() > 1){
          warning("Distribution is univariate, only values passed to the first argument are kept.")
        }
        data = ...elt(1)
      } else if (testMultivariate(self)) {
        if(...length() < 2){
          stop("Distribution is multivariate but values have only been passed to one argument.")
        } else {
          data = as.matrix(data.table(...))
        }
      } else {
        stop("Points to evaluate must be passed to `data` for matrixvariate distributions.")
      }
    }
  } else {
    if (testUnivariate(self)) {
      if (ncol(data) > 1) {
        warning("Distribution is univariate, only values in the first column are kept.")
      }
      data = data[, 1]
    } else if (testMultivariate(self)) {
      if(ncol(data) == 1){
        stop("Distribution is multivariate but values have only been passed to one argument.")
      } else {
        data = as.matrix(data)
      }
    } else {
      if (class(data) != "array") {
        stop("For multivariate distributions `data` must be an array.")
      }
    }
  }

  return(data)
}
pdqr_returner = function(pdqr, simplify){
  if (inherits(pdqr,"data.table")) {
    return(pdqr)
  } else{
    if (simplify) {
      return(pdqr)
    } else {
      pdqr = data.table(pdqr)
      colnames(pdqr) = self$short_name
      return(pdqr)
    }
  }
}
