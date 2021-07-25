#' @title Skewness Type
#' @description Gets the type of skewness
#' @details Skewness is a measure of asymmetry of a distribution.
#'
#' A distribution can either have negative skew, no skew or positive skew. A symmetric distribution
#' will always have no skew but the reverse relationship does not always hold.
#'
#' @param skew numeric
#'
#' @return Returns one of 'negative skew', 'no skew' or 'positive skew'.
#'
#' @examples
#' skewType(1)
#' skewType(0)
#' skewType(-1)
#' @export
skewType <- function(skew) {
  vapply(skew,
         function(.x) {
           if (is.nan(.x)) {
             "undefined"
           } else if (.x < 0) {
             "negative skew"
           } else if (.x == 0) {
             "no skew"
           } else {
             "positive skew"
           }
         }, character(1))
}
