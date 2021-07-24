#' @title Kurtosis Type
#' @description Gets the type of (excess) kurtosis
#' @details Kurtosis is a measure of the tailedness of a distribution. Distributions can be compared
#'  to the Normal distribution by whether their kurtosis is higher, lower or the same as that of the
#'  Normal distribution.
#'
#' A distribution with a negative excess kurtosis is called 'platykurtic', a distribution
#' with a positive excess kurtosis is called 'leptokurtic' and a distribution with an excess
#' kurtosis equal to zero is called 'mesokurtic'.
#'
#' @param kurtosis numeric.
#'
#' @seealso \code{\link{kurtosis}}, \code{\link{skewType}}
#'
#' @examples
#' exkurtosisType(-1)
#' exkurtosisType(0)
#' exkurtosisType(1)
#' @return Returns one of 'platykurtic', 'mesokurtic' or 'leptokurtic'.
#'
#' @export
exkurtosisType <- function(kurtosis) {
  vapply(kurtosis,
         function(.x) {
           if (is.nan(.x)) {
             "undefined"
           } else if (.x < 0) {
             "platykurtic"
           } else if (.x == 0) {
             "mesokurtic"
           } else {
             "leptokurtic"
           }
         }, character(1))
}
