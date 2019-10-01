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
  y = .distr6$wrappers[order(names(.distr6$wrappers))]
  if(simplify)
    return(names(y))
  else
    return(y)
}
