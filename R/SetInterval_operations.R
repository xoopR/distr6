#' @title Symbolic Operations for SetInterval
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name operation
#'
#' @usage operation(unicode,...)
#'
#' @param unicode unicode symbol for the operation.
#' @param ... sets and/or intervals to combine via the operation.
#'
#' @details Generally not recommended to use this function directly but instead
#'   via \code{\link{product}} or \code{\link{union}}.
#' @seealso \code{\link{SetInterval}}.
#' @export
operation <- function(unicode,lower,upper,type,dim,...){
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

  if(!is.null(lower)) lower = as.numeric(unlist(lapply(dots, function(x) x$inf())),recursive=T)
  if(!is.null(upper)) upper = as.numeric(unlist(lapply(dots, function(x) x$sup()),recursive = T))
  if(!is.null(dim)) dim = length(dots)
  if(!is.null(type)) type = "{}"

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = type, lower = lower,
                         upper = upper, dimension = dim))
}

#' @title Symbolic Cartesian Product for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name product
#'
#' @usage product(...)
#'
#' @param ... sets and/or intervals to take the cartesian product of.
#'
#' @details This does not calculate the cartesian product of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union}} for the union of two or more intervals/sets.
#' @export
product <- function(...){
  dots = list(...)
  if(length(unique(sapply(dots,function(x) x$getSymbol()))) == 1 & length(dots)>1)
    return(power(dots[[1]], length(dots)))
  else
    return(operation("\u00D7",...))
}

#' @title Symbolic Union for SetInterval
#'
#' @description Makes a symbolic representation for the union of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name union
#'
#' @usage union(...)
#'
#' @param ... sets and/or intervals to take the union of.
#'
#' @details This does not calculate the union of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product}} for the cartesian product of two or more intervals/sets.
#' @export
union <- function(...){
  operation("\u222A",...)
}

#' @title Symbolic Complement for SetInterval
#'
#' @description Makes a symbolic representation for the complement of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name complement
#'
#' @usage complement(...)
#'
#' @param ... sets and/or intervals to take the union of.
#'
#' @details This does not calculate the complement of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product}} and \code{\link{union}}.
#' @export
complement <- function(...){
  dots = list(...)

  x = dots[[1]]
  y = dots[[2]]

  if(inherits(x,"Interval") & inherits(y,"Interval")){
    if(y$sup() >= x$sup() & y$inf() <= x$inf())
      return(Empty$new())
    else if(y$inf() > x$sup()){
      lower <- x$inf()
      upper <- x$sup()
      type <- x$type()
    } else if(y$sup() < x$inf()){
      lower <- x$inf()
      upper <- x$sup()
      type <- x$type()
    } else if(y$sup() >= x$sup() & y$inf() > x$inf()){
      lower <- x$inf()
      upper <- y$inf()
      type <- paste0(substr(x$type(),1,1),")")
    } else if(y$sup() < x$sup() & y$inf() <= x$inf()){
      lower <- y$sup()
      upper <- x$sup()
      type <- paste0("(",substr(x$type(),2,2))
    }
  }
  operation("/",lower = lower, upper = upper, type = type, dim = x$dimension(),...)
}

#' @title Symbolic Exponentiation for SetInterval
#'
#' @description Makes a symbolic representation for the exponentiation of a given
#'   set or interval.
#' @return An R6 object of class SetInterval.
#' @name power
#'
#' @usage power(x, power)
#'
#' @param x set or interval
#' @param power power
#'
#' @details This does not calculate the value of exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @export
power <- function(x, power){
  symbol = paste0(x$getSymbol(),"^",power)
  lower = rep(x$inf(),power)
  upper = rep(x$sup(),power)

  SetInterval$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

#' @usage \method{^}{SetInterval}(x, power)
#' @rdname power
`^.SetInterval` <- function(x, power){
  power(x, power)
}

#' @usage \method{+}{SetInterval}(x, y)
#' @rdname union
#' @param x distribution
#' @param y distribution
`+.SetInterval` <- function(x, y){
  union(x, y)
}

#' @usage \method{*}{SetInterval}(x, y)
#' @rdname product
#' @param x distribution
#' @param y distribution
`*.SetInterval` <- function(x, y){
  product(x, y)
}

#' @usage \method{-}{SetInterval}(x, y)
#' @rdname complement
#' @param x distribution
#' @param y distribution
`-.SetInterval` <- function(x, y){
  complement(x, y)
}

#' @title Unicode Symbol of Special Sets
#'
#' @description Gets the unicode symbol for standard mathematical special sets.
#' @name setSymbol
#'
#' @usage setSymbol(set)
#'
#' @param set special set
#'
#' @details Special set can be supplied as a character string or class, case-insensitive.
#'   See \code{\link{SpecialSet}} for full list of currently supported sets.
#'
#' @seealso \code{\link{SpecialSet}}
#'
#' @export
setSymbol <- function(set){
  x = try(class(set),silent = T)
  if(inherits(x, "try-error"))
    set = paste0(substitute(set))
  else if(!inherits(set,"character"))
    set = paste0(substitute(set))
  set = tolower(set)
  return(switch(set,
                empty = "\u2205",
                naturals = "\u2115",
                posnaturals = "\u2115+",
                integers = "\u2124",
                posintegers = "\u2124+",
                negintegers = "\u2124-",
                rationals = "\u211A",
                posrationals = "\u211A+",
                negrationals = "\u211A-",
                reals = "\u211D",
                posreals = "\u211D+",
                negreals = "\u211D-",
                extendedreals = "\u211D \u222A {-\u221E, +\u221E}",
                complex = "\u2102"
  ))
}
