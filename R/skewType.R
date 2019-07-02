#' @title Skewness Type
#' @description Gets the type of skewness
#' @param skew numeric.
#' @examples
#' skewType(1)
#' @export
skewType <- function(skew){

  if(is.nan(skew)) return("undefined")

  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}
