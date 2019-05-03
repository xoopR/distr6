#' @title Symbolic Set Object
#'
#' @description An R6 set object for symbolic representation of mathematical sets.
#' @return \code{Set$new} constructs an R6 object of class Set.
#' @name Set
#'
#' @section Usage: Set$new(...)
#'
#' @param ... objects of any type to pass as set elements.
#'
#' @details All arguments passed to the constructor are elements in the set.
#'
#' @seealso The parent class \code{\link{SetInterval}} for a full list of inherited methods and variables.
NULL


#' @export
Set <- R6::R6Class("Set", inherit = SetInterval)
Set$set("public","initialize",function(...){
  if(missing(...))
    invisible(self)
  else{
    dots <- list(...)
    private$.setSymbol <- paste0("{",paste(dots,collapse = ", "),"}")
    private$.type <- "{}"
    private$.lower <- dots[[1]]
    private$.upper <- dots[[length(dots)]]
  }
  invisible(self)
})