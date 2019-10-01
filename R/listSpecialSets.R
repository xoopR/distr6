#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in SetInterval.
#' @param simplify logical. If FALSE (default) returns data.table of set name and symbol, otherwise set names as characters.
#' @seealso \code{\link{SpecialSet}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of \code{SpecialSet}s and their traits.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @export
listSpecialSets <- function(simplify = FALSE){
  y = c("Empty","Naturals","PosNaturals","Integers","PosIntegers","NegIntegers","Rationals",
        "PosRationals","NegRationals","Reals","PosReals","NegReals","ExtendedReals",
        "Complex")
  if(simplify)
    return(as.character(y))
  else{
    return(data.table::data.table(ClassName = y, Symbol = unname(sapply(y, setSymbol)),
                           Infimum = c("NULL","0","0/1","-Inf","0/1","-Inf","-Inf","0/1","-Inf","-Inf","0/1",rep("-Inf",3)),
                           Supremum = c("NULL",rep("Inf",4),"-1/0","Inf","Inf","-1/0","Inf","Inf","-1/0","Inf","Inf")))
  }
}
