#' @include SetInterval.R
#-------------------------------------------------------------
# Set Documentation
#-------------------------------------------------------------
#' @title Symbolic Set Object
#'
#' @description An R6 set object for symbolic representation of mathematical sets.
#'
#' @name Set
#'
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{...} \tab ANY \tab See constructor details. \cr
#' }
#'
#' @section Constructor Details: Objects of any class can be passed to the set constructor. If a numeric
#' vector is passed then a discrete set of integers is created in the range from the minimum to the
#' maximum input. Otherwise all arguments are parsed as elements in a mathematical set.
#'
#' @seealso The parent class \code{\link{SetInterval}} for a full list of inherited methods and variables.
#'
#' @export
NULL
#-------------------------------------------------------------
# Set Definition
#-------------------------------------------------------------
Set <- R6::R6Class("Set", inherit = SetInterval)
Set$set("public","initialize",function(..., dim = 1){
  if(length(list(...)) == 0){
    private$.type <- "{}"
    private$.lower <- NULL
    private$.upper <- NULL
    private$.setSymbol <- paste0("{}")
  } else{
    dots <- list(...)
    private$.elements <- unlist(dots)
    if(length(dots[[1]]) > 1 & is.numeric(dots[[1]])){
      private$.type <- "{}"
      private$.lower <- min(dots[[1]])
      private$.upper <- max(dots[[1]])
      private$.setSymbol <- paste0("{",private$.lower,",...,",private$.upper,"}")
    } else {
      private$.setSymbol <- paste0("{",paste(dots,collapse = ", "),"}")
      private$.type <- "{}"
      private$.lower <- dots[[1]]
      private$.upper <- dots[[length(dots)]]
    }
  }

  if(dim != 1)
    private$.setSymbol <- paste0(private$.setSymbol,"^",dim)

  private$.dimension <- dim

  invisible(self)
})
Set$set("public","length",function(){
  return(length(private$.elements))
})
Set$set("public","elements",function(){
  return(private$.elements)
})
Set$set("private",".class","integer")
Set$set("private",".elements",NULL)
Set$set("public","liesInSetInterval",function(x, all = FALSE, bound = NULL){
  ret = rep(FALSE, length(x))
  ret[x %in% self$elements()] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})

