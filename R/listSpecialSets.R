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
    symbols = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      zero = "zero" %in% names(formals(PosReals$public_methods$initialize))
      ClassName = x$classname
      x = x$new()
      Symbol = x$getSymbol()
      if(zero & grepl("Pos",ClassName))
        Infimum = "0/1"
      else
        Infimum = x$inf()
      if(is.null(Infimum)) Infimum = "NULL"
      if(zero & grepl("Neg",ClassName))
        Supremum = "-1/0"
      else
        Supremum = x$sup()
      if(is.null(Supremum)) Supremum = "NULL"
      return(cbind(ClassName, Symbol, Infimum, Supremum))
    }))
    row.names(symbols) = NULL

    return(data.table::data.table(symbols))
  }
}
