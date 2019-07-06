#' @title Unicode Symbol of Special Sets
#'
#' @description Gets the unicode symbol for standard mathematical special sets.
#' @name setSymbol
#'
#' @usage setSymbol(set)
#'
#' @param set special set
#'
#' @details Special set can be supplied as a character string or class, case-insensitive.
#'   See \code{\link{listSpecialSets}} for full list of currently supported sets.
#'
#' @seealso \code{\link{SpecialSet}}, \code{\link{listSpecialSets}}
#'
#' @examples
#' # Supplied as class
#' setSymbol(empty)
#'
#' # Supplied as string
#' setSymbol("empty")
#'
#' # Case-insensitive
#' setSymbol(EmPtY)
#'
#'
#' @export
setSymbol <- function(set){
  x = try(class(set),silent = T)
  if(inherits(x, "try-error"))
    set = paste0(substitute(set))
  else if(!inherits(set,"character"))
    set = paste0(substitute(set))
  set = tolower(set)
  return(switch(set,
                empty = "\u2205",
                naturals = "\u21150",
                posnaturals = "\u2115+",
                integers = "\u2124",
                posintegers = "\u2124+",
                negintegers = "\u2124-",
                rationals = "\u211A",
                posrationals = "\u211A+",
                negrationals = "\u211A-",
                reals = "\u211D",
                posreals = "\u211D+",
                negreals = "\u211D-",
                extendedreals = "\u211D \u222A {-\u221E, +\u221E}",
                complex = "\u2102"
  ))
}
