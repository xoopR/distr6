#' @title Lists Implemented Distribution Decorators
#' @description Lists decorators that can decorate an R6 Distribution.
#' @param simplify logical. If TRUE (default) returns results as characters, otherwise as R6 classes.
#' @seealso \code{\link{DistributionDecorator}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a list of \code{Decorator} classes.
#' @examples
#' listDecorators()
#' listDecorators(FALSE)
#' @export
listDecorators <- function(simplify = TRUE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "DistributionDecorator_generator")
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
