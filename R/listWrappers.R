#' @title Lists Implemented Distribution Wrappers
#' @description Lists wrappers that can wrap an R6 Distribution.
#' @param simplify logical. If TRUE (default) returns results as characters, otherwise as R6 classes.
#' @seealso \code{\link{DistributionWrapper}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a list of \code{Wrapper} classes.
#' @examples
#' listWrappers()
#' listWrappers(TRUE)
#' @export
listWrappers <- function(simplify = TRUE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "DistributionWrapper_generator" |
         environmentName(get(x)$get_inherit()) == "ProductDistribution_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE"]
  if(simplify)
    return(as.character(y))
  else
    return(lapply(y, get))
}
