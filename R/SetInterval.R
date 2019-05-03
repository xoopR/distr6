#' @title R6 Generalised Class for Symbolic Sets and Intervals
#'
#' @description The parent class to sets and intervals in R6.
#' @name SetInterval
#'
#' @section Usage: SetInterval$new(symbol, type, lower, upper, dimension)
#' @return \code{SetInterval$new} constructs an R6 object of class SetInterval.
#'
#' @param symbol string representation of set/interval.
#' @param type string for interval type, one of (), (], [), []
#' @param lower numeric lower limit of set/interval
#' @param upper numeric upper limit of set/interval
#' @param dimension numeric dimension of set/interval
#'
#' @details Whilst this is not an abstract class, direct construction is generally not advised.
#'   Construction should instead be called on 'Set' or 'Interval'
#'
#' @seealso \code{\link{Set}} for R6 set objects and \code{\link{Interval}} for R6 interval objects.
NULL


#' @export
SetInterval <- R6::R6Class("SetInterval")
SetInterval$set("public","initialize",function(symbol, type, lower, upper, dimension){
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  private$.dimension = dimension
  private$.setSymbol = symbol
  invisible(self)
})

#' @rdname SetInterval
#' @name lower
#' @section Usage: $lower()
#' @return \code{lower} gets lower limit of set/interval
SetInterval$set("public","lower",function(){
  return(private$.lower)
})

#' @rdname SetInterval
#' @name upper
#' @section Usage: $upper()
#' @return \code{upper} gets upper limit of set/interval
SetInterval$set("public","upper",function(){
  return(private$.upper)
})

#' @rdname SetInterval
#' @name type
#' @section Usage: $type()
#' @return \code{type} gets closed/open interval type
SetInterval$set("public","type",function(){
  return(private$.type)
})

#' @rdname SetInterval
#' @name dimension
#' @section Usage: $dimension()
#' @return \code{dimension} gets dimension of set/interval
SetInterval$set("public","dimension",function(){
  return(private$.dimension)
})

#' @rdname SetInterval
#' @name max
#' @section Usage: $max()
#' @return \code{max} gets maximum of set/interval
SetInterval$set("public","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$upper()-.Machine$double.eps)
  else
    return(self$upper())
})

#' @rdname SetInterval
#' @name min
#' @section Usage: $min()
#' @return \code{min} gets minimum of set/interval
SetInterval$set("public","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$lower()+.Machine$double.eps)
  else
    return(self$lower())
})

#' @rdname SetInterval
#' @name sup
#' @section Usage: $sup()
#' @return \code{sup} gets supremum of set/interval
SetInterval$set("public","sup",function(){
  return(self$upper())
})

#' @rdname SetInterval
#' @name inf
#' @section Usage: inf()
#' @return \code{inf} gets infimum of set/interval
SetInterval$set("public","inf",function(){
  return(self$lower())
})

#' @rdname SetInterval
#' @name getSymbol
#' @section Usage: $getSymbol()
#' @return \code{getSymbol} gets symbolic representation of set/interval
SetInterval$set("public","getSymbol",function() return(private$.setSymbol))

#' @rdname SetInterval
#' @name print.SetInterval
#' @section Usage: $print()
#' @return \code{print} prints symbolic representation of set/interval
SetInterval$set("public","print",function(){
  print(self$getSymbol())
})

SetInterval$set("private",".lower",NULL)
SetInterval$set("private",".upper",NULL)
SetInterval$set("private",".type",NULL)
SetInterval$set("private",".dimension",NULL)
SetInterval$set("private",".setSymbol",NULL)