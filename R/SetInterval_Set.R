#' @include SetInterval.R
#-------------------------------------------------------------
# Set Documentation
#-------------------------------------------------------------
#' @title R6 Generalised Class for Symbolic Sets
#'
#' @description A symbolic R6 Set class.
#'
#' @details Sets are distinguished from intervals in R6 as they are finite mathematical sets with
#' elements that can be printed. The elements can be of any class.
#'
#' @name Set
#'
#' @section Constructor: Set$new(..., dim = 1)
#'
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{...} \tab ANY \tab Elements in the set. \cr
#'    \code{dim} \tab integer \tab Dimension of the set.
#'}
#'
#'@section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
#'   \code{type()} \tab  \code{\link{type.SetInterval}}  \cr
#'   \code{dimension()} \tab \code{\link{dimension.SetInterval}}\cr
#'   \code{max()} \tab  \code{\link{max.SetInterval}} \cr
#'   \code{min()} \tab  \code{\link{min.SetInterval}} \cr
#'   \code{sup()} \tab  \code{\link{sup.SetInterval}}  \cr
#'   \code{inf()} \tab  \code{\link{inf.SetInterval}}  \cr
#'   \code{getSymbol()} \tab  \code{\link{getSymbol.SetInterval}} \cr
#'   \code{class()} \tab  \code{\link{class.SetInterval}}  \cr
#'   \code{elements()} \tab  \code{\link{elements}}  \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Set Methods} \tab \strong{Link} \cr
#'   \code{length()} \tab  \code{\link{length.Set}}  \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSetInterval(x, all = FALSE, bound = FALSE)} \tab \code{\link{liesInSetInterval}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{print()} \tab \code{\link[base]{print}} \cr
#'   }
#'
#' @return Returns an R6 object of class Set.
#'
#' @seealso \code{\link{Interval}}
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

#' @name length.Set
#' @rdname length.Set
#' @title Length of Set
#' @description Returns the length of the Set as the number of elements.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Number of elements in the set.
#' @section R6 Usage: $length()
#' @seealso \code{\link{Set}}
Set$set("public","length",function(){
  return(length(private$.elements))
})

#' @name elements
#' @rdname elements
#' @title Set Elements Accessor
#' @description Returns the elements in a Set.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Elements in the set.
#' @section R6 Usage: $elements()
#' @seealso \code{\link{Set}}
Set$set("public","elements",function(){
  return(private$.elements)
})
Set$set("private",".class","integer")
Set$set("private",".elements",NULL)
Set$set("public","liesInSetInterval",function(x, all = FALSE, bound = FALSE){
  ret = rep(FALSE, length(x))
  ret[x %in% self$elements()] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})

