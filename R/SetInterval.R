#' @title R6 Generalised Class for Symbolic Sets and Intervals
#'
#' @description The parent class to sets and intervals in R6.
#' @return An R6 object of class SetInterval.
#' @name SetInterval
#'
#' @usage \code{SetInterval$new(symbol, type, lower, upper, dimension)}
#'
#' @section Constructor Arguments:
#' \code{symbol} string representation of set/interval.
#'
#' \code{type} string for interval type, one of (), (], [), []
#'
#' \code{lower} numeric lower limit of set/interval
#'
#' \code{upper} numeric upper limit of set/interval
#'
#' \code{dimension} numeric dimension of set/interval
#'
#' @section Methods:
#'
#' \code{$lower()} Get lower limit of set/interval
#'
#' \code{$upper()} Get upper limit of set/interval
#'
#' \code{$type()} Get closed/open interval type
#'
#' \code{$dimension()} Get dimension of set/interval
#'
#' \code{$max()} Get maximum of set/interval
#'
#' \code{$min()} Get minimum of set/interval
#'
#' \code{$sup()} Get supremum of set/interval
#'
#' \code{$inf()} Get infimum of set/interval
#'
#' \code{$getSymbol()} Get symbolic representation of set/interval
#'
#' \code{$print()} Print symbolic representation of set/interval
#'
#' @details Whilst this is not an abstract class, direct construction is generally not advised.
#'   Construction should instead be called on 'sets' or 'interval'
#'
#' @seealso \code{\link{set}} for R6 set objects and \code{\link{interval}} for R6 interval objects.
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
SetInterval$set("public","lower",function(){
  return(private$.lower)
})
SetInterval$set("public","upper",function(){
  return(private$.upper)
})
SetInterval$set("public","type",function(){
  return(private$.type)
})
SetInterval$set("public","dimension",function(){
  return(private$.dimension)
})
SetInterval$set("public","max",function(){
  if(private$.type %in% c("()","[)"))
    return(self$upper()-.Machine$double.eps)
  else
    return(self$upper())
})
SetInterval$set("public","min",function(){
  if(private$.type %in% c("()","(]"))
    return(self$lower()+.Machine$double.eps)
  else
    return(self$lower())
})
SetInterval$set("public","sup",function(){
  return(self$upper())
})
SetInterval$set("public","inf",function(){
  return(self$lower())
})
SetInterval$set("public","getSymbol",function() return(private$.setSymbol))
SetInterval$set("public","print",function(){
  print(self$getSymbol())
})
SetInterval$set("private",".lower",NULL)
SetInterval$set("private",".upper",NULL)
SetInterval$set("private",".type",NULL)
SetInterval$set("private",".dimension",NULL)
SetInterval$set("private",".setSymbol",NULL)