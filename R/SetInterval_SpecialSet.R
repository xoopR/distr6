#' @title Special Mathematical Sets
#'
#' @description R6 representations of the 'special' mathematical sets.
#' @return Returns an R6 object of the given class.
#' @name SpecialSet
#'
#' @usage SpecialSet$new(dim = 1, lower = -Inf, upper = Inf, type = "()", ...)
#'
#' @param dim numeric dimension of set.
#' @param lower lower limit of the set.
#' @param upper upper limit of the set.
#' @param type interval type, one of "()","(]","[)","[]".
#' @param zero logical; if FALSE (default), zero is not included in the set.
#'
#' @format
#'
#' @details Whilst these could all inherit from the class Interval. By convention we instead use Set.
#'
#' @seealso The parent class \code{\link{SetInterval}} for a full list of inherited methods and variables.
NULL


#' @export
SpecialSet <- R6::R6Class("SpecialSet", inherit = Set)
SpecialSet$set("public","initialize",function(dim = 1, lower = -Inf,
                                              upper = Inf, type = "()", ...){
  if(dim!=1)
    private$.setSymbol <- paste0(setSymbol(paste0(getR6Class(self))),"^",dim)
  else
    private$.setSymbol <- setSymbol(paste0(getR6Class(self)))
  private$.lower <- lower
  private$.upper <- upper
  private$.type <- type
  private$.dimension <- dim
  invisible(self)
})

#' @rdname SpecialSet
#' @usage Naturals$new(dim = 1)
Naturals <- R6::R6Class("Naturals",inherit = SpecialSet)
Naturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 0, type = "[)")
})

#' @rdname SpecialSet
#' @usage PosNaturals$new(dim = 1)
PosNaturals <- R6::R6Class("PosNaturals",inherit = SpecialSet)
PosNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage Integers$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
Integers <- R6::R6Class("Integers",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage PosIntegers$new(dim = 1)
PosIntegers <- R6::R6Class("PosIntegers",inherit = SpecialSet)
PosIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage NegIntegers$new(dim = 1)
NegIntegers <- R6::R6Class("NegIntegers",inherit = SpecialSet)
NegIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

#' @rdname SpecialSet
#' @usage Rationals$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage PosRationals$new(dim = 1)
PosRationals <- R6::R6Class("PosRationals",inherit = Rationals)
PosRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage NegRationals$new(dim = 1)
NegRationals <- R6::R6Class("NegRationals",inherit = Rationals)
NegRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

#' @rdname SpecialSet
#' @usage Reals$new(dim = 1, lower = -Inf, upper = Inf, type=  "()")
Reals <- R6::R6Class("Reals",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage PosReals$new(dim = 1, zero = FALSE)
PosReals <- R6::R6Class("PosReals",inherit = Reals)
PosReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 0, type = "()")
})

#' @rdname SpecialSet
#' @usage NegReals$new(dim = 1, zero = FALSE)
NegReals <- R6::R6Class("NegReals",inherit = Reals)
NegReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = 0, type = "()")
})

#' @rdname SpecialSet
#' @usage ExtendedReals$new(dim = 1)
ExtendedReals <- R6::R6Class("ExtendedReals",inherit = Reals)
ExtendedReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})

#' @rdname SpecialSet
#' @usage Complex$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
Complex <- R6::R6Class("Complex",inherit = SpecialSet)