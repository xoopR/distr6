#' @title Lists Implemented Distribution Decorators
#' @description Lists decorators that can decorate an R6 Distribution.
#' @param simplify logical. If `TRUE` (default) returns results as characters,
#' otherwise as R6 classes.
#' @seealso \code{\link{DistributionDecorator}}
#' @return Either a list of characters (if `simplify` is `TRUE`) or a list of
#' [DistributionDecorator] classes.
#' @examples
#' listDecorators()
#' listDecorators(FALSE)
#' @export
listDecorators <- function(simplify = TRUE) {
  y <- .distr6$decorators[order(names(.distr6$decorators))]
  if (simplify) {
    return(names(y))
  } else {
    return(y)
  }
}
