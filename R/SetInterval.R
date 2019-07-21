#-------------------------------------------------------------
# SetInterval Documentation
#-------------------------------------------------------------
#' @title R6 Generalised Class for Symbolic Sets and Intervals
#'
#' @description A generic SetInterval class primarily used as the parent class to \code{Set} and \code{Interval}.
#'
#' @name SetInterval
#'
#' @section Constructor: SetInterval$new(symbol, lower, upper, type, class = "numeric", dimension)
#'
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{symbol} \tab character \tab String representation of SetInterval. \cr
#'    \code{lower} \tab numeric \tab Lower limit of SetInterval. \cr
#'    \code{upper} \tab numeric \tab Upper limit of SetInterval. \cr
#'    \code{type} \tab character \tab Interval type, one of (), (], [), []. \cr
#'    \code{class} \tab character \tab Atomic class, one of "numeric" or "integer". \cr
#'    \code{dimension} \tab integer \tab Dimension of SetInterval.
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
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSetInterval(x, all = FALSE, bound = FALSE)} \tab \code{\link{liesInSetInterval}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{print()} \tab \code{\link[base]{print}} \cr
#'   }
#'
#' @details Whilst this is not an abstract class, direct construction is generally not advised.
#'   Construction should instead be called on 'Set' or 'Interval'.
#'
#' @return Returns an R6 object of class SetInterval.
#'
#' @seealso \code{\link{Set}} for R6 Set objects and \code{\link{Interval}} for R6 Interval objects.
#'
#' @export
NULL
#-------------------------------------------------------------
# SetInterval Definition
#-------------------------------------------------------------
SetInterval <- R6::R6Class("SetInterval")
SetInterval$set("public","initialize",function(symbol, lower, upper, type, class = "numeric", dimension){
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  private$.dimension = as.integer(dimension)
  private$.setSymbol = symbol
  private$.class = class
  invisible(self)
})

#' @name type.SetInterval
#' @rdname type.SetInterval
#' @title SetInterval Type Accessor
#' @description Returns the SetInterval 'type', one of "()","(]","[]","[)".
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Returns the type of SetInterval, one of "()","(]","[]","[)".
#' @section R6 Usage: $type()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","type",function(){
  return(private$.type)
})

#' @name dimension.SetInterval
#' @rdname dimension.SetInterval
#' @title SetInterval Dimension Accessor
#' @description Returns the SetInterval dimension.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $dimension()
#' @return Dimension as an integer.
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","dimension",function(){
  return(private$.dimension)
})

#' @name max.SetInterval
#' @rdname max.SetInterval
#' @title SetInterval Maximum Accessor
#' @description Returns the SetInterval maximum.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Maximum as a numeric.
#' @section R6 Usage: $max()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$sup()-1.1e-15)
  else
    return(self$sup())
})

#' @name min.SetInterval
#' @rdname min.SetInterval
#' @title SetInterval Minimum Accessor
#' @description Returns the SetInterval minimum.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Minimum as a numeric.
#' @section R6 Usage: $min()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$inf()+1.1e-15)
  else
    return(self$inf())
})

#' @name sup.SetInterval
#' @rdname sup.SetInterval
#' @title SetInterval Supremum Accessor
#' @description Returns the SetInterval supremum.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @return Supremum as a numeric.
#' @section R6 Usage: $sup()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","sup",function(){
  return(private$.upper)
})

#' @name inf.SetInterval
#' @rdname inf.SetInterval
#' @title SetInterval Infimum Accessor
#' @description Returns the SetInterval infimum.
#' @return Infimum as a numeric.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $inf()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","inf",function(){
  return(private$.lower)
})

#' @name getSymbol.SetInterval
#' @rdname getSymbol.SetInterval
#' @title SetInterval Symbol Accessor
#' @description Returns the SetInterval symbol.
#' @return Returns string representation of a SetInterval.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $getSymbol()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","getSymbol",function(){
  return(private$.setSymbol)
})

SetInterval$set("public","print",function(){
  print(self$getSymbol())
})

#' @name class.SetInterval
#' @rdname class.SetInterval
#' @title SetInterval Minimum Accessor
#' @description Returns the SetInterval minimum.
#' @return One of 'numeric' or 'integer'.
#' @details This is an R6 method only, no S3 dispatch is available.
#' @section R6 Usage: $min()
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","class",function(){
  return(private$.class)
})

#' @name liesInSetInterval
#' @rdname liesInSetInterval
#' @title Test if Data Lies in SetInterval.
#' @description Tests if the given data lies in the SetInterval, either tests if all data lies in the type
#' or any of it, can choose if bounds should be included.
#' @return Either a vector of logicals if \code{all} is FALSE otherwise returns TRUE if every element
#' lies in the SetInterval or FALSE otherwise.
#' @details
#' If \code{all} is TRUE (default) returns TRUE only if every element in x lies in the type. If \code{all}
#' is FALSE then returns a vector of logicals for each corresponding element in the vector x.
#'
#' If called on a set, then the \code{bound} argument is ignored and returns TRUE if x is an element
#' in the set.
#'
#' This is an R6 method only, no S3 dispatch is available.
#'
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) tests against dmin/dmax otherwise inf/sup.
#'
#' @section R6 Usage: $liesInSetInterval(x, all = FALSE, bound = FALSE)
#' @seealso \code{\link{SetInterval}}
SetInterval$set("public","liesInSetInterval",function(x, all = FALSE, bound = FALSE){
  ret = rep(FALSE, length(x))

  if(self$class() == "integer")
    class_test = sapply(x, checkmate::testIntegerish)
  else if(self$class() == "numeric")
    class_test = sapply(x, checkmate::testNumeric)

  if(bound)
    ret[(x >= self$inf() & x <= self$sup() & class_test)] = TRUE
  else
    ret[(x >= self$min() & x <= self$max() & class_test)] = TRUE

  if(all)
    return(all(ret))
  else
    return(ret)
})

SetInterval$set("private",".lower",NULL)
SetInterval$set("private",".upper",NULL)
SetInterval$set("private",".type",NULL)
SetInterval$set("private",".class",NULL)
SetInterval$set("private",".dimension",NULL)
SetInterval$set("private",".setSymbol",NULL)
