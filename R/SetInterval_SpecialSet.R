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
NULL

#' @include SetInterval_Set.R
#' @export
SpecialSet <- R6::R6Class("SpecialSet", inherit = Set)
SpecialSet$set("public","initialize",function(dim = 1, lower = -Inf,
                                              upper = Inf, type = "()", complement = NULL, ...){
  if(RSmisc::getR6Class(self, pos = environment()) == "SpecialSet")
    stop(paste(RSmisc::getR6Class(self, pos = environment()), "is an abstract class that can't be initialized."))

  if(dim!=1){
    if(!is.null(complement))
      private$.setSymbol <- paste0(paste0(setSymbol(paste0(RSmisc::getR6Class(self, pos = environment()))),"^",dim),"/",complement$getSymbol())
    else
      private$.setSymbol <- paste0(setSymbol(paste0(RSmisc::getR6Class(self, pos = environment()))),"^",dim)
  } else{
    if(!is.null(complement))
      private$.setSymbol <- paste0(setSymbol(paste0(RSmisc::getR6Class(self, pos = environment()))),"/",complement$getSymbol())
    else
      private$.setSymbol <- setSymbol(paste0(RSmisc::getR6Class(self, pos = environment())))
  }
  private$.lower <- lower
  private$.upper <- upper
  private$.type <- type
  private$.dimension <- dim
  invisible(self)
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
NULL
#' @export
Naturals <- R6::R6Class("Naturals",inherit = SpecialSet)
Naturals$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim, lower = 0, type = "[)")
})

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
NULL
#' @export
PosNaturals <- R6::R6Class("PosNaturals",inherit = SpecialSet)
PosNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1, type = "[)")
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
NULL
#' @export
Integers <- R6::R6Class("Integers",inherit = SpecialSet)
Integers$set("public", "initialize", function(dim = 1,lower = -Inf, upper = Inf, ...){
  super$initialize(dim, type = "[]", lower = lower, upper = upper)
})

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
NULL
#' @export
PosIntegers <- R6::R6Class("PosIntegers",inherit = SpecialSet)
PosIntegers$set("public", "initialize", function(dim = 1, zero = FALSE,
                                                 lower = NULL, upper = Inf){
  if(!is.null(lower))
    super$initialize(dim, lower = lower, upper = upper, type = "[)",
                     complement = Set$new(0:(lower-1)))
  else{
    if(zero)
      super$initialize(dim, lower = 0, type = "[)")
    else
      super$initialize(dim, lower = 1, type = "[)")
  }
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
NULL
#' @export
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)
Rationals$set("public", "initialize", function(dim = 1,...){
  super$initialize(dim, type = "()")
})

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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
NULL
#' @export
Reals <- R6::R6Class("Reals",inherit = SpecialSet)
Reals$set("public", "initialize", function(dim = 1,type, ...){
  super$initialize(dim, type = "()",...)
})

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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
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
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim = 1} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
NULL
#' @export
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
NULL
#' @export
Complex <- R6::R6Class("Complex",inherit = SpecialSet)
Complex$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})
