#' @title Kurtosis Type
#' @description Gets the type of (excess) kurtosis
#' @param kurtosis numeric.
#' @examples
#' exkurtosisType(1)
#' @export
exkurtosisType <- function(kurtosis){

  if(is.nan(kurtosis)) return("undefined")

  if(kurtosis < 0)
    return("platykurtic")
  else if(kurtosis == 0)
    return("mesokurtic")
  else
    return("leptokurtic")
}
