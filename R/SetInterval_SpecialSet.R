#' @title Special Mathematical Sets
#'
#' @description R6 representations of the 'special' mathematical sets.
#' @return An R6 object of class SpecialSet.
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
#'
#' @details All arguments passed to the constructor are elements in the set. See
#'   \code{\link{SetInterval}} for inherited methods and variables.
#'
#' @inheritSection SetInterval Methods
#'
#' @seealso The parent classes \code{\link{set}} and \code{\link{SetInterval}}.
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
#' @usage naturals$new(dim = 1)
naturals <- R6::R6Class("naturals",inherit = SpecialSet)
naturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 0, type = "[)")
})

#' @rdname SpecialSet
#' @usage posNaturals$new(dim = 1)
posNaturals <- R6::R6Class("posNaturals",inherit = SpecialSet)
posNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage integers$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
integers <- R6::R6Class("integers",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage posIntegers$new(dim = 1)
posIntegers <- R6::R6Class("posIntegers",inherit = SpecialSet)
posIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage negIntegers$new(dim = 1)
negIntegers <- R6::R6Class("negIntegers",inherit = SpecialSet)
negIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

#' @rdname SpecialSet
#' @usage rationals$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
rationals <- R6::R6Class("rationals",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage posRationals$new(dim = 1)
posRationals <- R6::R6Class("posRationals",inherit = rationals)
posRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

#' @rdname SpecialSet
#' @usage negRationals$new(dim = 1)
negRationals <- R6::R6Class("negRationals",inherit = rationals)
negRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

#' @rdname SpecialSet
#' @usage reals$new(dim = 1, lower = -Inf, upper = Inf, type=  "()")
reals <- R6::R6Class("reals",inherit = SpecialSet)

#' @rdname SpecialSet
#' @usage posReals$new(dim = 1, zero = FALSE)
posReals <- R6::R6Class("posReals",inherit = reals)
posReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 0, type = "()")
})

#' @rdname SpecialSet
#' @usage negReals$new(dim = 1, zero = FALSE)
negReals <- R6::R6Class("negReals",inherit = reals)
negReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = 0, type = "()")
})

#' @rdname SpecialSet
#' @usage extendedReals$new(dim = 1)
extendedReals <- R6::R6Class("extendedReals",inherit = reals)
extendedReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})

#' @rdname SpecialSet
#' @usage complex$new(dim = 1, lower = -Inf, upper = Inf, type = "()")
complex <- R6::R6Class("complex",inherit = SpecialSet)