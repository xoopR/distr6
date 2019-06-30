#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in SetInterval.
#' @param simplify logical. If FALSE (default) returns data.table of set name and symbol, otherwise character.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @export
listSpecialSets <- function(simplify = FALSE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "SpecialSet_generator" |
         environmentName(get(x)$get_inherit()) == "Rationals_generator" |
         environmentName(get(x)$get_inherit()) == "Reals_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE"]
  if(simplify)
    return(as.character(y))
  else{
    symbols = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      x = x$new()
      Symbol = x$getSymbol()
      Infimum = x$inf()
      if(is.null(Infimum)) Infimum = "NULL"
      Supremum = x$sup()
      if(is.null(Supremum)) Supremum = "NULL"
      return(cbind(ClassName, Symbol, Infimum, Supremum))
    }))
    row.names(symbols) = NULL

    return(data.table::data.table(symbols))
  }
}
