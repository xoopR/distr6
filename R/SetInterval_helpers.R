#' @title Symbolic Operations for SetInterval
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name operation
#'
#' @usage \code{operation(unicode,...)}
#'
#' @param \code{unicode} unicode symbol for the operation.
#' @param \code{...} sets and/or intervals to combine via the operation.
#'
#' @details Generally not recommended to use this function directly but instead
#'   via \code{\link{product}} or \code{\link{union}}.
#' @seealso The parent class \code{\link{SetInterval}}.
#' @export
operation <- function(unicode,...){
  dots = list(...)
  symbols = lapply(dots,function(x){
    if(inherits(x,"R6ClassGenerator"))
      x <- x[["new"]]()
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })
  lower = as.numeric(lapply(dots, function(x) x$lower()))
  upper = as.numeric(lapply(dots, function(x) x$upper()))

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = "{}", lower = lower,
                         upper = upper, dimension = length(dots)))
}

#' @title Symbolic Cartesian Product for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name product
#'
#' @usage \code{product(...)}
#'
#' @param \code{...} sets and/or intervals to take the cartesian prodcut of.
#'
#' @details This does not calculate the cartesian product of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union}} for the union of two or more intervals/sets.
#' @export
product <- function(...){
  operation("\u00D7",...)
}

#' @title Symbolic Operations for SetInterval
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name operation
#'
#' @usage \code{operation(unicode,...)}
#'
#' @param \code{unicode} unicode symbol for the operation.
#' @param \code{...} sets and/or intervals to combine via the operation.
#'
#' @details Generally not recommended to use this function directly but instead
#'   via \code{\link{product}} or \code{\link{union}}.
#' @seealso The parent class \code{\link{SetInterval}}.
#' @export
operation <- function(unicode,...){
  dots = list(...)
  symbols = lapply(dots,function(x){
    if(inherits(x,"R6ClassGenerator"))
      x <- x[["new"]]()
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })
  lower = as.numeric(lapply(dots, function(x) x$lower()))
  upper = as.numeric(lapply(dots, function(x) x$upper()))

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = "{}", lower = lower,
                         upper = upper, dimension = length(dots)))
}

#' @title Symbolic Union for SetInterval
#'
#' @description Makes a symbolic representation for the union of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name union
#'
#' @usage \code{union(...)}
#'
#' @param \code{...} sets and/or intervals to take the union of.
#'
#' @details This does not calculate the union of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product}} for the cartesian product of two or more intervals/sets.
#' @export
union <- function(...){
  operation("\u222A",...)
}

#' @title Symbolic Exponentiation for SetInterval
#'
#' @description Makes a symbolic representation for the exponentiation of a given
#'   set or interval.
#' @return An R6 object of class SetInterval.
#' @name power
#'
#' @usage \code{power(x, power)}
#'
#' @param \code{x} set or interval
#' @param \code{power} power
#'
#' @details This does not calculate the value of exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @export
power <- function(x, power){
  symbol = paste0(x$getSymbol(),"^",power)
  lower = rep(x$lower(),power)
  upper = rep(x$upper(),power)

  SetInterval$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

#' @usage x^power
#' @rdname power
`^.SetInterval` <- function(x, power){
  power(x, power)
}

#' @usage x+y
#' @rdname union
`+.SetInterval` <- function(x, y){
  union(x, y)
}

#' @usage x*y
#' @rdname product
`*.SetInterval` <- function(x, y){
  product(x, y)
}

#' @title Unicode Symbol of Special Sets
#'
#' @description Gets the unicode symbol for standard mathematical special sets.
#' @name setSymbol
#'
#' @usage \code{setSymbol(set)}
#'
#' @param \code{set} special set
#'
#' @details Special set should be supplied as a character string.
#'   See \code{\link{SpecialSet}} for full list of currently supported sets.
#'
#' @seealso \code{\link{SpecialSet}}
#'
#' @export
setSymbol <- function(set){
  if(!inherits(set,"character"))
    set = paste0(substitute(set))
  return(switch(set,
                naturals = "\u2115",
                posNaturals = "\u2115+",
                integers = "\u2124",
                posIntegers = "\u2124+",
                negIntegers = "\u2124-",
                rationals = "\u211A",
                posRationals = "\u211A+",
                negRationals = "\u211A-",
                reals = "\u211D",
                posReals = "\u211D+",
                negReals = "\u211D-",
                extendedReals = "\u211D \u222A {-\u221E, +\u221E}",
                complex = "\u2102"
  ))
}