#' @include SetInterval.R
#-------------------------------------------------------------
# Interval Documentation
#-------------------------------------------------------------
#' @title Symbolic Interval Object
#'
#' @description An R6 set object for symbolic representation of mathematical intervals.
#'
#' @name Interval
#'
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{lower = -Inf} \tab numeric \tab Lower limit of set/interval. \cr
#'    \code{upper = Inf} \tab numeric \tab Upper limit of set/interval. \cr
#'    \code{type = "[]"} \tab character \tab Interval type, one of (), (], [), []. \cr
#' }
#'
#' @section Coercion Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#' \code{as.numeric} \tab numeric \tab Coerces interval range to numeric vector. \cr
#' }
#'
#' @seealso The parent class \code{\link{SetInterval}} for a full list of inherited methods and variables.
NULL
#-------------------------------------------------------------
# Interval Definition
#-------------------------------------------------------------
#' @export
Interval <- R6::R6Class("Interval", inherit = SetInterval)
Interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]", class = "numeric", dim = 1){
  types = c("()","(]","[]","[)")
  stopifnot(type %in% types)
  stopifnot(lower<=upper)
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  if(lower == -Inf) lower = "-\u221E"
  if(upper == Inf) upper = "+\u221E"
  private$.setSymbol <- paste0(substr(type,1,1),lower,":", upper,substr(type,2,2))
  private$.class <- class
  if(dim != 1)
    private$.setSymbol <- paste0(private$.setSymbol,"^",dim)
  invisible(self)
})

Interval$set("public","as.numeric",function(){
  return(seq.int(self$min(),self$max(),1))
})
Interval$set("public","length",function(){
  if(self$inf() == -Inf | self$sup() == Inf)
    return(Inf)
  else
    return(length(self$as.numeric()))
})
Interval$set("private",".class",NULL)
