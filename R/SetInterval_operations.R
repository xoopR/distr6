#' @include RSmisc_helpers.R

#' @title Symbolic Operations for SetInterval
#'
#' @description Operations for SetInterval objects and subclasses, symbolic only.
#' @return An R6 object of class SetInterval.
#' @name operation
#'
#' @param unicode unicode symbol for the operation.
#' @param ... sets and/or intervals to combine via the operation.
#' @param lower lower bound of new SetInterval
#' @param upper upper bound of new SetInterval
#' @param type type of new SetInterval
#' @param dim dimension of new SetInterval
#'
#' @details Generally not recommended to use this function directly but instead
#'   via \code{\link{product.SetInterval}} or \code{\link{union.SetInterval}}.
#' @seealso \code{\link{SetInterval}}.
#' @export
operation <- function(unicode,...,lower=NULL,upper=NULL,type=NULL,dim=NULL){
  dots = list(...)
  symbols = lapply(dots,function(x){
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })

  if(is.null(lower)) lower = as.numeric(unlist(lapply(dots, function(x) x$inf())))
  if(is.null(upper)) upper = as.numeric(unlist(lapply(dots, function(x) x$sup())))
  if(is.null(dim)) dim = length(dots)
  if(is.null(type)) type = "{}"

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(SetInterval$new(symbol = setSymbol, type = type, lower = lower,
                         upper = upper, dimension = dim))
}

#' @title Symbolic Cartesian product.SetInterval for SetInterval
#'
#' @description Makes a symbolic representation for the cartesian product.SetInterval of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name product.SetInterval
#'
#' @usage product.SetInterval(...)
#'
#' @param ... sets and/or intervals to take the cartesian product.SetInterval of.
#'
#' @details This does not calculate the cartesian product.SetInterval of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{union.SetInterval}} for the union.SetInterval of two or more intervals/sets.
#' @export
product.SetInterval <- function(...){
  dots = list(...)
  if(length(unique(sapply(dots,function(x) x$getSymbol()))) == 1 & length(dots)>1)
    return(power.SetInterval(dots[[1]], length(dots)))
  else
    return(operation("\u00D7",...))
}

#' @title Symbolic union.SetInterval for SetInterval
#'
#' @description Makes a symbolic representation for the union.SetInterval of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name union.SetInterval
#'
#' @param ... sets and/or intervals to take the union.SetInterval of.
#' @param dim dimension of new SetInterval.
#'
#' @details This does not calculate the union.SetInterval of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}} for the cartesian product.SetInterval of two or more intervals/sets.
#' @export
union.SetInterval <- function(..., dim = 1){
  if(length(unique(unlist(lapply(list(...),function(y) y$getSymbol())))) != 1){
    lower = min(sapply(list(...), function(y) y$inf()))
    upper = max(sapply(list(...), function(y) y$sup()))
    operation("\u222A",...,dim = dim, lower = lower, upper = upper)
  }else
    return(list(...)[[1]])
}

#' @title Symbolic complement.SetInterval for SetInterval
#'
#' @description Makes a symbolic representation for the complement.SetInterval of sets/intervals.
#' @return An R6 object of class SetInterval.
#' @name complement.SetInterval
#'
#' @usage complement.SetInterval(...)
#'
#' @param ... sets and/or intervals to take the union.SetInterval of.
#'
#' @details This does not calculate the complement.SetInterval of the arguments but
#'   is just a symbolic representation using unicode.
#'
#' @seealso \code{\link{product.SetInterval}} and \code{\link{union.SetInterval}}.
#' @export
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
    else if(y$inf() == x$inf() & y$sup() == y$inf())
      return(Interval$new(lower = x$inf(), upper = x$sup(), type = paste0("(",substr(x$type(),2,2)),
                          class = x$class()))
    else if(y$sup() == x$sup() & y$sup() == y$inf())
      return(Interval$new(lower = x$inf(), upper = x$sup(), type = paste0(substr(x$type(),1,1),")"),,
             class = x$class()))
    else if(y$sup() <= x$sup() & y$inf() >= x$inf())
      return(union.SetInterval(Interval$new(x$inf(),y$inf(),type=paste0(substr(x$type(),1,1),")"),class = x$class()),
                     Interval$new(y$sup(),x$sup(),type=paste0("(",substr(x$type(),2,2)),class = x$class()),
                     dim = x$dimension()))
  }

 # operation("/",lower = lower, upper = upper, type = type, dim = x$dimension(),x,y)
}

#' @title Symbolic Exponentiation for SetInterval
#'
#' @description Makes a symbolic representation for the exponentiation of a given
#'   set or interval.
#' @return An R6 object of class SetInterval.
#' @name power.SetInterval
#'
#' @usage power.SetInterval(x, power.SetInterval)
#'
#' @param x set or interval
#' @param power.SetInterval power.SetInterval
#'
#' @details This does not calculate the value of exponentiation but
#'   is just a symbolic representation using unicode.
#'
#' @export
power.SetInterval <- function(x, power.SetInterval){
  symbol = paste0(x$getSymbol(),"^",power.SetInterval)
  lower = rep(x$inf(),power.SetInterval)
  upper = rep(x$sup(),power.SetInterval)

  SetInterval$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power.SetInterval)
}

#' @usage \method{^}{SetInterval}(x, power.SetInterval)
#' @rdname power.SetInterval
#' @export
`^.SetInterval` <- function(x, power.SetInterval){
  power.SetInterval(x, power.SetInterval)
}

#' @usage \method{+}{SetInterval}(x, y)
#' @rdname union.SetInterval
#' @param x distribution
#' @param y distribution
#' @export
`+.SetInterval` <- function(x, y){
  union.SetInterval(x, y)
}

#' @usage \method{*}{SetInterval}(x, y)
#' @rdname product.SetInterval
#' @param x distribution
#' @param y distribution
#' @export
`*.SetInterval` <- function(x, y){
  product.SetInterval(x, y)
}

#' @usage \method{-}{SetInterval}(x, y)
#' @rdname complement.SetInterval
#' @param x distribution
#' @param y distribution
#' @export
`-.SetInterval` <- function(x, y){
  complement.SetInterval(x, y)
}

