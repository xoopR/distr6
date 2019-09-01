#' @include SetInterval.R
#-------------------------------------------------------------
# Interval Documentation
#-------------------------------------------------------------
#' @title R6 Generalised Class for Symbolic Intervals
#'
#' @description A symbolic R6 Interval class.
#'
#' @details Intervals are distinguished from sets in R6 as they can take an infinite range and are defined
#' over a continuous range (albeit integer or numeric).
#'
#' @name Interval
#'
#' @section Constructor: Interval$new(lower = -Inf, upper = Inf, type = "[]", class = "numeric", dim = 1)
#'
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{lower} \tab numeric \tab Lower limit of interval. \cr
#'    \code{upper} \tab numeric \tab Upper limit of interval. \cr
#'    \code{type} \tab character \tab Interval type, one of (), (], [), []. \cr
#'    \code{class} \tab character \tab Atomic class, one of "numeric" or "integer". \cr
#'    \code{dim} \tab integer \tab Dimension of SetInterval.
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
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Interval Methods} \tab \strong{Link} \cr
#'   \code{length()} \tab  \code{\link{length.Interval}}  \cr
#'   \code{as.numeric()} \tab  \code{\link{as.numeric.Interval}}  \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSetInterval(x, all = FALSE, bound = FALSE)} \tab \code{\link{liesInSetInterval}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{print()} \tab \code{\link[base]{print}} \cr
#'   }
#'
#' @return Returns an R6 object of class Interval.
#'
#' @seealso \code{\link{Set}}
#'
#' @export
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
  private$.setSymbol <- paste0(substr(type,1,1),lower,",", upper,substr(type,2,2))
  private$.class <- class
  if(dim != 1)
    private$.setSymbol <- paste0(private$.setSymbol,"^",dim)
  invisible(self)
})

#' @name as.numeric.Interval
#' @rdname as.numeric.Interval
#' @title Coerces Interval to Numeric
#' @description If possible coerces an integer-class to numeric.
#' @details
#' If the interval is of class 'integer', returns the interval as a numeric. Otherwise does nothing.
#'
#' This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $length()
#'
#'@return If the Interval is of class integer then returns a numeric vector of the interval elements.
#'
#' @seealso \code{\link{Set}}, \code{\link{length.Interval}}
Interval$set("public","as.numeric",function(){
  if(self$class() == "integer")
    return(seq.int(self$min(),self$max(),1))
})

#' @name length.Interval
#' @rdname length.Interval
#' @title Length of Interval
#' @description Returns the length of the Interval after coercing to numeric.
#' @details
#' If the interval is of class 'numeric', returns Inf. Otherwise coerces to numeric and returns that
#' length.
#'
#' @return Returns the length of interval as a numeric if class 'integer' otherwise Inf.
#'
#' This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $length()
#'
#' @seealso \code{\link{Set}}, \code{\link{length.Interval}}
Interval$set("public","length",function(){
  if(self$inf() == -Inf | self$sup() == Inf)
    return(Inf)
  if(self$class() == "numeric")
    return(Inf)

  return(length(self$as.numeric()))
})
Interval$set("private",".class",NULL)
