#' @include SetInterval_Interval.R
#'
#' @title Special Mathematical Sets
#'
#' @description Abstract class for the representation of the 'special' mathematical sets.
#'
#' @name SpecialSet
#'
#' @details This is an abstract class that cannot be constructed, instead construct one of the child classes,
#' see below.
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
SpecialSet <- R6::R6Class("SpecialSet", inherit = Interval)
SpecialSet$set("public","initialize",function(dim = 1, lower = -Inf,
                                              upper = Inf, type = "()", ...){
  if(getR6Class(self, pos = environment()) == "SpecialSet")
    stop(paste(getR6Class(self, pos = environment()), "is an abstract class that can't be initialized."))

  if(dim!=1)
      private$.setSymbol <- paste0(setSymbol(paste0(getR6Class(self, pos = environment()))),"^",dim)
  else
      private$.setSymbol <- setSymbol(paste0(getR6Class(self, pos = environment())))

  private$.lower <- lower
  private$.upper <- upper
  private$.type <- type
  private$.dimension <- dim
  invisible(self)
})
SpecialSet$set("private",".class","integer")
#' @title Empty Set
#' @description The mathematical empty, or null, set.
#' @name Empty
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Empty <- R6::R6Class("Empty",inherit = SpecialSet)
Empty$set("public", "initialize", function(){
  super$initialize(dim = 1, lower = NULL, upper = NULL, type = "{}")
})

#' @title Set of Natural Numbers
#' @description The mathematical set of natural numbers.
#' @name Naturals
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Naturals <- R6::R6Class("Naturals",inherit = SpecialSet)
Naturals$set("public", "initialize", function(dim = 1, lower = 0){
  super$initialize(dim, lower = lower, type = "[)")
})
Naturals$set("private",".class","integer")

#' @title Set of Positive Natural Numbers
#' @description The mathematical set of positive natural numbers.
#' @name PosNaturals
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
PosNaturals <- R6::R6Class("PosNaturals",inherit = Naturals)
PosNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1)
})

#' @title Set of Integers
#' @description The mathematical set of integers.
#' @name Integers
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Integers <- R6::R6Class("Integers",inherit = SpecialSet)
Integers$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim,...)
})
Integers$set("private",".class","integer")

#' @title Set of Positive Integers
#' @description The mathematical set of positive integers.
#' @name PosIntegers
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
PosIntegers <- R6::R6Class("PosIntegers",inherit = Integers)
PosIntegers$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, lower = 0, type = "[)")
  else
    super$initialize(dim, lower = 1, type = "[)")
})

#' @title Set of Negative Integers
#' @description The mathematical set of negative integers.
#' @name NegIntegers
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
NegIntegers <- R6::R6Class("NegIntegers",inherit = Integers)
NegIntegers$set("public", "initialize", function(dim = 1, zero = FALSE){
  if(zero)
    super$initialize(dim, upper = 0, type = "(]")
  else
    super$initialize(dim, upper = -1, type = "(]")
})

#' @title Set of Rationals
#' @description The mathematical set of rationals.
#' @name Rationals
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)
Rationals$set("public", "initialize", function(dim = 1, ...){
  super$initialize(dim,...)
})
Rationals$set("private",".class","numeric")

#' @title Set of Positive Rationals
#' @description The mathematical set of positive rationals.
#' @name PosRationals
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Reals <- R6::R6Class("Reals",inherit = SpecialSet)
Reals$set("public", "initialize", function(dim = 1, ...){
  super$initialize(dim, ...)
})
Reals$set("private",".class","numeric")

#' @title Set of Positive Reals
#' @description The mathematical set of positive reals.
#' @name PosReals
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
ExtendedReals <- R6::R6Class("ExtendedReals",inherit = Reals)
ExtendedReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})

#' @title Set of Complex Numbers
#' @description The mathematical set of complex numbers.
#' @name Complex
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @export
NULL
Complex <- R6::R6Class("Complex",inherit = SpecialSet)
Complex$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})
Complex$set("private",".class","complex")
