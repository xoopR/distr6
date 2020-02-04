#' @name listSpecialSets-Deprecated
#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in SetInterval.
#' @param simplify logical. If FALSE (default) returns data.table of set name and symbol, otherwise set names as characters.
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of \code{SpecialSet}s and their traits.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @keywords internal
NULL

#' @name listSpecialSets
#' @rdname distr6-deprecated
#' @export
NULL
listSpecialSets <- function(...){
  .Deprecated("set6::listSpecialSets", "distr6", "listSpecialSets has been deprecated, use set6::listSpecialSets instead.")
}
