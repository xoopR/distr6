#' @title Symbolic Set Object
#'
#' @description An R6 set object for symbolic representation of mathematical sets.
#' @return An R6 object of class set.
#' @name set
#'
#' @usage \code{Set$new(...)}
#'
#' @section Constructor Arguments:
#' \code{...} objects of any type to pass as set elements.
#'
#' @inheritSection SetInterval Methods
#'
#' @details All arguments passed to the constructor are elements in the set. See
#'   \code{\link{SetInterval}} for inherited methods and variables.
#'
#' @seealso The parent class \code{\link{SetInterval}}.
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