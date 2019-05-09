#' @title String Representation of Print
#' @name strprint
#' @description Gets the result of a print method as a character string without printing. Useful for
#' methods such as \code{cat}, \code{summary} and \code{print}. Additionally can be used to easily
#' parse R6 objects into data-frames.
#'
#' @param x R6 object
#' @param ... Additional arguments
#' @usage strprint(x,...)
#'
#' @export
strprint <- function(x,...){
  UseMethod("strprint", x)
}

#' @rdname strprint
#' @export
strprint.R6 <- function(x,...){
  return(x$strprint())
}

#' @rdname strprint
#' @export
strprint.list <- function(x,...){
  return(lapply(x,strprint))
}

#' @title Get R6 Class Name
#' @description Gets the name of the given R6 class.
#' @param x R6 object.
#' @return string giving R6 class name.
#'
#' @examples
#' getR6Class(Binomial$new())
#'
#' @export
getR6Class <- function(x){
  return(get(class(x)[[1]])$classname)
}