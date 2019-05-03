#' @title Symbolic Interval Object
#'
#' @description An R6 set object for symbolic representation of mathematical intervals.
#' @return \code{Interval$new} constructs an R6 object of class Interval
#' @name Interval
#'
#' @section Usage: Interval$new(lower = -Inf, upper = Inf, type = "[]")
#'
#' @param lower lower limit of the interval.
#' @param upper upper limit of the interval.
#' @param type endpoint type of the interval.
#'
#'
#' @seealso The parent class \code{\link{SetInterval}} for a full list of inherited methods and variables.
NULL


#' @export
Interval <- R6::R6Class("Interval", inherit = SetInterval)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]"){
  types = c("()","(]","[]","[)")
  stopifnot(type %in% types)
  stopifnot(lower<=upper)
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  if(lower == -Inf) lower = "-\u221E"
  if(upper == Inf) upper = "+\u221E"
  private$.setSymbol <- paste0(substr(type,1,1),lower,":", upper,substr(type,2,2))
  invisible(self)
})

#' @rdname Interval
#' @name numeric
#' @section Usage: $numeric()
#' @return \code{numeric} gets numeric vector of the interval range.
Interval$set("public","numeric",function(){
  return(seq.int(self$min(),self$max(),1))
})