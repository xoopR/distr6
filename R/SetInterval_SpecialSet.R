#' @title Special Mathematical Sets
#'
#' @description Abstract class for the representation of the 'special' mathematical sets.
#' @name SpecialSet
#'
#' @return error. Cannot be constructed.
#'
#' @details This is an abstract class that cannot be constructed, instead construct one of the child classes, see below.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL

#' @include SetInterval_Set.R
#' @export
SpecialSet <- R6::R6Class("SpecialSet", inherit = Set)
SpecialSet$set("public","initialize",function(dim = 1, lower = -Inf,
                                              upper = Inf, type = "()", ...){
  if(getR6Class(self) == "SpecialSet")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

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

#' @title Set of Natural Numbers
#' @description The mathematical set of natural numbers.
#' @name Naturals
#' @section Usage: Naturals$new(dim = 1)
#' @return Returns an R6 object of class Naturals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
Naturals <- R6::R6Class("Naturals",inherit = SpecialSet)
Naturals$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim, lower = 0, type = "[)")
})

#' @title Set of Positive Natural Numbers
#' @description The mathematical set of positive natural numbers.
#' @name PosNaturals
#' @section Usage: PosNaturals$new(dim = 1)
#' @return Returns an R6 object of class PosNaturals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
PosNaturals <- R6::R6Class("PosNaturals",inherit = SpecialSet)
PosNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1, type = "[)")
})

#' @title Set of Integers
#' @description The mathematical set of integers.
#' @name Integers
#' @section Usage: Integers$new(dim = 1)
#' @return Returns an R6 object of class Integers.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
Integers <- R6::R6Class("Integers",inherit = SpecialSet)
Integers$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim, type = "[]")
})

#' @title Set of Positive Integers
#' @description The mathematical set of positive integers.
#' @name PosIntegers
#' @section Usage: PosIntegers$new(dim = 1)
#' @return Returns an R6 object of class PosIntegers.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
PosIntegers <- R6::R6Class("PosIntegers",inherit = SpecialSet)
PosIntegers$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 1, type = "[)")
})

#' @title Set of Negative Integers
#' @description The mathematical set of negative integers.
#' @name NegIntegers
#' @section Usage: NegIntegers$new(dim = 1)
#' @return Returns an R6 object of class NegIntegers.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
NegIntegers <- R6::R6Class("NegIntegers",inherit = SpecialSet)
NegIntegers$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = -1, type = "(]")
})

#' @title Set of Rationals
#' @description The mathematical set of rationals.
#' @name Rationals
#' @section Usage: Rationals$new(dim = 1)
#' @return Returns an R6 object of class Rationals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)
Rationals$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim, type = "()")
})

#' @title Set of Positive Rationals
#' @description The mathematical set of positive rationals.
#' @name PosRationals
#' @section Usage: PosRationalsnew(dim = 1)
#' @return Returns an R6 object of class PosRationals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
PosRationals <- R6::R6Class("PosRationals",inherit = Rationals)
PosRationals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 0, type = "()")
})

#' @title Set of Negative Rationals
#' @description The mathematical set of negative rationals.
#' @name NegRationals
#' @section Usage: NegRationals$new(dim = 1)
#' @return Returns an R6 object of class NegRationals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
NegRationals <- R6::R6Class("NegRationals",inherit = Rationals)
NegRationals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = 0, type = "()")
})

#' @title Set of Reals
#' @description The mathematical set of reals.
#' @name Reals
#' @section Usage: Reals$new(dim = 1)
#' @return Returns an R6 object of class Reals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
Reals <- R6::R6Class("Reals",inherit = SpecialSet)
Reals$set("public", "initialize", function(dim = 1,type, ...){
  super$initialize(dim, type = "()",...)
})

#' @title Set of Positive Reals
#' @description The mathematical set of positive reals.
#' @name PosReals
#' @section Usage: PosReals$new(dim = 1)
#' @return Returns an R6 object of class PosReals.
#' @param dim numeric dimension of set.
#' @param zero logical; if FALSE (default) zero is not included in the set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
PosReals <- R6::R6Class("PosReals",inherit = Reals)
PosReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 0, type = "()")
})

#' @title Set of Negative Reals
#' @description The mathematical set of negative reals.
#' @name NegReals
#' @section Usage: NegReals$new(dim = 1)
#' @return Returns an R6 object of class NegReals.
#' @param dim numeric dimension of set.
#' @param zero logical; if FALSE (default) zero is not included in the set
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
NegReals <- R6::R6Class("NegReals",inherit = Reals)
NegReals$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = 0, type = "()")
})

#' @title Set of Extended Reals
#' @description The mathematical set of extended reals.
#' @name ExtendedReals
#' @section Usage: ExtendedReals$new(dim = 1)
#' @return Returns an R6 object of class ExtendedReals.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
ExtendedReals <- R6::R6Class("ExtendedReals",inherit = Reals)
ExtendedReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})

#' @title Set of Complex Numbers
#' @description The mathematical set of complex numbers.
#' @name Complex
#' @section Usage: Complex$new(dim = 1)
#' @return Returns an R6 object of class Complex.
#' @param dim numeric dimension of set.
#'
#' @seealso \code{\link{Naturals}}, \code{\link{PosNaturals}}, \code{\link{Integers}},
#'   \code{\link{PosIntegers}}, \code{\link{NegIntegers}}, \code{\link{Rationals}}, \code{\link{PosRationals}},
#'   \code{\link{NegRationals}}, \code{\link{Reals}}, \code{\link{PosReals}}, \code{\link{NegReals}},
#'   \code{\link{ExtendedReals}}, \code{\link{Complex}}.
NULL
#' @export
Complex <- R6::R6Class("Complex",inherit = SpecialSet)
Complex$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})