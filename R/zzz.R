## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib distr6, .registration = TRUE
## usethis namespace: end
NULL

#' @importFrom R6 R6Class
#' @importFrom data.table data.table as.data.table
#' @importFrom checkmate assert
#' @importFrom stats setNames
#' @import set6
#' @import param6
#' @import ooplah
NULL

globalVariables(c("pdf", "cdf"))
