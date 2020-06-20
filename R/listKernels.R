#' @title Lists Implemented Kernels
#' @description Lists all implemented kernels in distr6.
#' @param simplify logical. If FALSE (default) returns kernels with support as a data.table,
#' otherwise returns kernel names as characters.
#' @seealso \code{\link{Kernel}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of
#' \code{Kernel}s and their traits.
#' @examples
#' listKernels()
#' @export
listKernels <- function(simplify = FALSE) {
  kerns <- .distr6$kernels[order(.distr6$kernels$ClassName), ]
  if (simplify) {
    return(unlist(kerns$ClassName))
  } else {
    return(kerns)
  }
}
