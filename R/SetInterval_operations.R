#' @include RSmisc_helpers.R

#' @title Symbolic Operations for SetInterval
#'
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name setOperation
#'
#' @param unicode unicode symbol for the setOperation.
#' @param sets list of sets and/or intervals to combine via the setOperation.
#' @param lower lower bound of new SetInterval
#' @param upper upper bound of new SetInterval
#' @param type type of new SetInterval
#' @param dim dimension of new SetInterval
#'
#' @details Generally not recommended to use this function directly but instead
#'   via one of the implemented operations.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{union.SetInterval}}, \code{\link{complement.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @export
setOperation <- function(unicode,sets,lower=NULL,upper=NULL,type=NULL,dim=NULL){
  symbols = lapply(sets,function(x){
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })

  if(is.null(lower)) lower = as.numeric(unlist(lapply(sets, function(x) x$inf())))
  if(is.null(upper)) upper = as.numeric(unlist(lapply(sets, function(x) x$sup())))
  if(is.null(dim)) dim = length(sets)
  if(is.null(type)) type = "{}"

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = type, lower = lower,
                         upper = upper, dimension = dim))
}

#' @title Symbolic Cartesian Product for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product of sets/intervals.
#'
#' @name product.SetInterval
#'
#' @usage product.SetInterval(...)
#'
#' @param ... SetIntervals to take the cartesian product of.
#'
#' @details This does not calculate the cartesian product of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union.SetInterval}}, \code{\link{complement.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() * Reals$new()
#' product.SetInterval(PosNaturals$new(), Reals$new())
#'
#' @export
NULL
product.SetInterval <- function(...){
  dots = list(...)
  if(length(unique(sapply(dots,function(x) x$getSymbol()))) == 1 & length(dots)>1)
    return(power.SetInterval(dots[[1]], length(dots)))
  else
    return(setOperation("\u00D7", sets = dots))
}

#' @title Symbolic Unions for SetInterval
#'
#' @description Makes a symbolic representation for the union of sets/intervals.
#'
#' @name union.SetInterval
#'
#' @usage union.SetInterval(..., dim = 1)
#'
#' @param ... SetIntervals to take the union of.
#' @param dim dimension of new SetInterval.
#'
#' @details This does not calculate the union of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{complement.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() * Reals$new()
#' product.SetInterval(PosNaturals$new(), Reals$new())
#'
#' @export
NULL
union.SetInterval <- function(..., dim = 1){
  dots = list(...)

  if(length(dots) == 1)
    return(dots[[1]])

  class = lapply(dots, getR6Class)
  parClass = lapply(class, function(x) get(x)$inherit)
  dots = dots[!(parClass %in% class)]

  if(length(unique(unlist(lapply(dots,function(y) y$getSymbol())))) != 1){
    lower = min(sapply(dots, function(y) y$inf()))
    upper = max(sapply(dots, function(y) y$sup()))
    setOperation("\u222A", sets = dots, dim = dim, lower = lower, upper = upper)
  }else
    return(dots[[1]])
}

#' @title Symbolic Complement for SetInterval
#'
#' @description Makes a symbolic representation for the complement of sets/intervals.
#'
#' @name complement.SetInterval
#'
#' @usage complement.SetInterval(...)
#'
#' @param ... SetIntervals to take the complement of.
#'
#' @details This does not calculate the complement of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{union.SetInterval}},
#' \code{\link{power.SetInterval}}
#'
#' @examples
#' PosNaturals$new() - Interval$new(-1,10)
#' complement.SetInterval(Reals$new(), Interval$new(10,Inf))
#'
#' @export
NULL
complement.SetInterval <- function(...){
  dots = list(...)

  x = dots[[1]]
  y = dots[[2]]

  if(inherits(y,"Set")){
    if(y$length()==1)
      y <- Interval$new(y$elements(),y$elements())
  }

  if(inherits(x,"Interval") & inherits(y,"Interval")){
    if(y$sup() >= x$sup() & y$inf() <= x$inf())
      return(Empty$new())
    else if(y$inf() > x$sup() | y$sup() < x$inf())
      return(x)
    else if(y$sup() >= x$sup() & y$inf() > x$inf() & y$inf() <= x$sup())
      return(Interval$new(lower = x$inf(), upper = y$inf(), type = paste0(substr(x$type(),1,1),")"),
                          class = x$class()))
    else if(y$sup() < x$sup() & y$inf() <= x$inf() & y$sup() >= x$inf())
      return(Interval$new(lower = y$sup(), upper = x$sup(), type = paste0("(",substr(x$type(),2,2)),
                          class = x$class()))
    else if(y$sup() <= x$sup() & y$inf() >= x$inf())
      return(union.SetInterval(Interval$new(x$inf(),y$inf(),type=paste0(substr(x$type(),1,1),")"),class = x$class()),
                     Interval$new(y$sup(),x$sup(),type=paste0("(",substr(x$type(),2,2)),class = x$class()),
                     dim = x$dimension()))
  }

 # setOperation("/",lower = lower, upper = upper, type = type, dim = x$dimension(),x,y)
}

#' @title Symbolic Exponentiation for SetInterval
#'
#' @description Makes a symbolic representation for the exponentiation of a given set/interval.
#'
#' @name power.SetInterval
#'
#' @usage power.SetInterval(x, power)
#'
#' @param x SetInterval
#' @param power power to raise SetInterval to
#'
#' @details This does not calculate the exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}}, \code{\link{union.SetInterval}},
#' \code{\link{complement.SetInterval}}
#'
#' @examples
#' PosNaturals$new() ^ 2
#' power.SetInterval(Reals$new(), 3)
#'
#' @export
NULL
power.SetInterval <- function(x, power){
  symbol = paste0(x$getSymbol(),"^",power)
  lower = rep(x$inf(),power)
  upper = rep(x$sup(),power)

  SetInterval$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

#' @usage \method{^}{SetInterval}(x, power)
#' @rdname power.SetInterval
#' @export
`^.SetInterval` <- function(x, power){
  power.SetInterval(x, power)
}

#' @usage \method{+}{SetInterval}(x, y)
#' @rdname union.SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
`+.SetInterval` <- function(x, y){
  union.SetInterval(x, y)
}

#' @usage \method{*}{SetInterval}(x, y)
#' @rdname product.SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
`*.SetInterval` <- function(x, y){
  product.SetInterval(x, y)
}

#' @usage \method{-}{SetInterval}(x, y)
#' @rdname complement.SetInterval
#' @param x SetInterval
#' @param y SetInterval
#' @export
`-.SetInterval` <- function(x, y){
  complement.SetInterval(x, y)
}

