#' @title Skewness Type
#' @description Gets the type of skewness
#' @details Skewness is a measure of asymmetry of a distribution.
#'
#' A distribution can either have negative skew, no skew or positive skew. A symmetric distribution
#' will always have no skew but the reverse relationship does not always hold.
#'
#' @param skew numeric.
#'
#' @seealso \code{\link{skewness}}, \code{\link{exkurtosisType}}
#'
#' @return Returns one of 'negative skew', 'no skew' or 'positive skew'.
#'
#' @examples
#' skewType(1)
#' skewType(0)
#' skewType(-1)
#' @export
skewType <- function(skew) {

  if (is.nan(skew)) {
    return("undefined")
  }

  if (skew < 0) {
    return("negative skew")
  } else if (skew == 0) {
    return("no skew")
  } else {
    return("positive skew")
  }
}
