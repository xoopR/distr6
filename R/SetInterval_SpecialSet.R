#' @include SetInterval_Interval.R setSymbol.R
#'
#' @title Special Mathematical Sets
#'
#' @description Abstract class for the representation of the 'special' mathematical sets.
#'
#' @name SpecialSet
#'
#' @details
#' Special sets refer to the most commonly used (and important) sets in mathematics. Including
#' the sets of natural numbers, integers and reals.
#'
#' This is an abstract class that cannot be constructed, instead construct one of the implemented
#' SpecialSet child-classes. For a full list of these see \code{\link{listSpecialSets}}.
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

#' @name Empty
#' @title Empty Set
#' @description The mathematical Empty, or Null, set.
#' @details The Empty set is the set defined by having no elements,
#' \deqn{Empty = \{ \}}{Empty = { }}
#'
#' @section Constructor: Empty$new()
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Empty$new()
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
#'
#' @details The set of Naturals is defined as the counting numbers, i.e.
#' \deqn{Naturals = \{0, 1, 2,...\}}{Naturals = {0, 1, 2,...}}
#'
#' @section Constructor: Naturals$new(dim = 1, lower = 0)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{lower} \tab integer \tab Where to start the set. \cr
#' }
#'
#' @section Constructor Details: Generally the \code{lower} argument should be ignored, its primary use-case
#' is for the \code{PosNaturals} child-class.
#'
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Naturals$new()
#' Naturals$new(dim = 2)
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
#'
#' @details The set of Positive Naturals is defined as the positive counting numbers, i.e.
#' \deqn{PosNaturals = \{1, 2, 3,...\}}{PosNaturals = {1, 2, 3,...}}
#'
#' @section Constructor: PosNaturals$new(dim = 1)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' PosNaturals$new()
#' PosNaturals$new(dim = 2)
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
#'
#' @details The set of Integers is defined as the set of numbers that can be written without a fractional
#' component, i.e.
#' \deqn{Integers = \{...,-3, -2, -1, 0, 1, 2, 3,...\}}{Integers = {...,-3, -2, -1, 0, 1, 2, 3,...}}
#'
#' @section Constructor: Integers$new(dim = 1,...)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{...} \tab ANY \tab Additional arguments.
#' }
#'
#' @section Constructor Details: Generally the \code{...} argument should be ignored, its primary use-case
#' is for the child-classes.
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Integers$new()
#' Integers$new(dim = 2)
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
#'
#' @details The set of PosIntegers is defined as the set of positive or non-negative numbers that can
#' be written without a fractional component, i.e.
#' \deqn{PosIntegers = \{0, 1, 2, 3,...\}}{PosIntegers = {0, 1, 2, 3,...}}
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: PosIntegers$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' PosIntegers$new()
#' PosIntegers$new(zero = TRUE)
#' PosIntegers$new(dim = 2)
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
#'
#' @details The set of NegIntegers is defined as the set of negative or non-positive numbers that can
#' be written without a fractional component, i.e.
#' \deqn{NegIntegers = \{...,-3, -2, -1, 0\}}{NegIntegers = {...,-3, -2, -1, 0}}
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: NegIntegers$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' NegIntegers$new()
#' NegIntegers$new(zero = TRUE)
#' NegIntegers$new(dim = 2)
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
#' @description The mathematical set of rational numbers.
#' @name Rationals
#'
#' @details The set of Rationals is defined as the set of numbers that can be written as a fraction
#' of two integers, i.e.
#' \deqn{Rationals = \{\frac{p}{q} | p,q \ \in \ Z\}}{Rationals = {p/q | p,q \epsilon Z}}
#' where \eqn{Z} is the set of integers.
#'
#' @section Constructor: Rationals$new(dim = 1,...)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{...} \tab ANY \tab Additional arguments.
#' }
#'
#' @section Constructor Details: Generally the \code{...} argument should be ignored, its primary use-case
#' is for the child-classes.
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Rationals$new()
#' Rationals$new(dim = 2)
#'
#' @export
NULL
Rationals <- R6::R6Class("Rationals",inherit = SpecialSet)
Rationals$set("public", "initialize", function(dim = 1, ...){
  super$initialize(dim,...)
})
Rationals$set("private",".class","numeric")

#' @title Set of Positive Rationals
#' @description The mathematical set of positive rational numbers.
#' @name PosRationals
#'
#' @details The set of Positive Rationals is defined as the set of numbers that can be written as a fraction
#' of two integers and are non-negative, i.e.
#' \deqn{PosRationals = \{\frac{p}{q} | p,q \ \in \ Z, \ \frac{p}{q} \ge 0\}}{PosRationals = {p/q | p,q \epsilon Z, p/q \ge 0}}
#' where \eqn{Z} is the set of integers.
#'
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: PosRationals$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' PosRationals$new()
#' PosRationals$new(zero = TRUE)
#' PosRationals$new(dim = 2)
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
#' @description The mathematical set of negative rational numbers.
#' @name NegRationals
#'
#' @details The set of Positive Rationals is defined as the set of numbers that can be written as a fraction
#' of two integers and are non-negative, i.e.
#' \deqn{NegRationals = \{\frac{p}{q} | p,q \ \in \ Z, \ \frac{p}{q} \le 0\}}{NegRationals = {p/q | p,q \epsilon Z, p/q \le 0}}
#' where \eqn{Z} is the set of integers.
#'
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: NegRationals$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' NegRationals$new()
#' NegRationals$new(zero = TRUE)
#' NegRationals$new(dim = 2)
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
#' @description The mathematical set of real numbers.
#' @name Reals
#'
#' @details The set of Reals is defined as the union of the set of rationals and irrationals, i.e.
#' \deqn{Reals = I \cup Q}{Reals = I \cup Q}
#' where \eqn{I} is the set of irrationals and \eqn{Q} is the set of rationals.
#'
#' @section Constructor: Reals$new(dim = 1,...)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{...} \tab ANY \tab Additional arguments.
#' }
#'
#' @section Constructor Details: Generally the \code{...} argument should be ignored, its primary use-case
#' is for the child-classes.
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Reals$new()
#' Reals$new(dim = 2)
#'
#' @export
NULL
Reals <- R6::R6Class("Reals",inherit = SpecialSet)
Reals$set("public", "initialize", function(dim = 1, ...){
  super$initialize(dim, ...)
})
Reals$set("private",".class","numeric")

#' @title Set of Positive Reals
#' @description The mathematical set of positive real numbers.
#' @name PosReals
#'
#' @details The set of Positive Reals is defined as the union of the set of positive rationals and positive
#' irrationals, i.e.
#' \deqn{PosReals = I^+ \cup Q^+}{PosReals = I+ \cup Q+}
#' where \eqn{I^+}{I+} is the set of positive irrationals and \eqn{Q^+}{Q+} is the set of positive rationals.
#'
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: PosReals$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' PosReals$new()
#' PosReals$new(zero = TRUE)
#' PosReals$new(dim = 2)
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
#' @description The mathematical set of negative real numbers.
#' @name NegReals
#'
#' @details The set of Negative Reals is defined as the union of the set of negative rationals and negative
#' irrationals, i.e.
#' \deqn{NegReals = I^- \cup Q^-}{NegReals = I- \cup Q-}
#' where \eqn{I^-}{I-} is the set of negative irrationals and \eqn{Q^-}{Q-} is the set of negative rationals.
#'
#' \eqn{0} may or may not be included (depending on the \code{zero} argument).
#'
#' @section Constructor: NegReals$new(dim = 1, zero = FALSE)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#'    \code{zero = FALSE} \tab logical \tab If TRUE, zero is included in the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' NegReals$new()
#' NegReals$new(zero = TRUE)
#' NegReals$new(dim = 2)
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
#' @description The mathematical set of extended real numbers.
#' @name ExtendedReals
#'
#' @details The set of Extended Reals is defined as the union of the set of reals with +-Infinity, i.e.
#' \deqn{ExtendedReals = R \cup \{-\infty, \infty\}}
#' where \eqn{R} is the set of reals.
#'
#' @section Constructor: ExtendedReals$new(dim = 1)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' ExtendedReals$new()
#' ExtendedReals$new(dim = 2)
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
#'
#' @details The set of Complex numbers is defined as the set of reals with possibly imaginary components, i.e.
#' \deqn{Complex = \{a + bi \ | \ a,b \in R\}}{Complex = {a + bi | a,b \epsilon R}}
#' where \eqn{R} is the set of reals.
#'
#' @section Constructor: Complex$new(dim = 1)
#' @section Constructor Arguments:
#'  \tabular{lll}{
#'    \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'    \code{dim} \tab numeric \tab Dimension of the set. \cr
#' }
#'
#' @seealso \code{\link{listSpecialSets}}
#'
#' @examples
#' Complex$new()
#' Complex$new(dim = 2)
#'
#' @export
NULL
Complex <- R6::R6Class("Complex",inherit = SpecialSet)
Complex$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})
Complex$set("private",".class","complex")
