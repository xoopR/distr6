assertThat <- function(x, cond, errormsg){
  if(cond)
    invisible(x)
  else
    stop(errormsg)
}
checkThat <- function(cond, errormsg){
  if(cond)
    return(TRUE)
  else
    return(errormsg)
}
testThat <- function(cond){
  if(cond)
    return(TRUE)
  else
    return(FALSE)
}
isThat <- function(cond){
  return(testThat(cond))
}
getR6Class <- function(object, classname = TRUE, n.par = 0, pos = -1){
  if(classname)
    return(get(class(object)[[n.par+1]], pos = pos)$classname)
  else
    return(get(class(object)[[n.par+1]], pos = pos))
}
ifnerror <- function(expr, noerror, error = NULL, silent = T){
  x = try(expr, silent)
  if(inherits(x, "try-error")){
    if(is.null(error) | error == "warn")
      stopwarn("warn", "Error not Nerror!")
    else if(error == "stop")
      stopwarn("stop", "Error not Nerror!")
    else
      error
  } else {
    noerror
  }
}
makeChecks <- function(assertionName, cond, errormsg, args = alist(x=),
                       pos = -1){
  cond = substitute(cond)
  errormsg = substitute(errormsg)
  value = function(x){}
  formals(value) = args
  body(value) = substitute(assertThat(x,arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("assert",assertionName), value = value,
         pos = pos)

  body(value) = substitute(checkThat(arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("check",assertionName), value = value,
         pos = pos)

  body(value) = substitute(testThat(arg1),list(arg1=cond))
  assign(paste0("test",assertionName), value = value,
         pos = pos)

  body(value) = substitute(isThat(arg1),list(arg1=cond))
  assign(paste0("is",assertionName), value = value,
         pos = pos)
}
stopwarn <- function(error = "warn", error.msg){
  checkmate::assert(error == "warn", error == "stop",
                    .var.name = "'error' should be one of 'warn' or 'stop'.")
  if(error == "stop")
    stop(error.msg)
  else{
    warning(error.msg, call. = F)
    return(NULL)
  }
}

#' @title String Representation of Print
#' @name strprint
#' @description Extends S3 dispatch of strprint to lists and any R6 class.
#' @details strprint is a suggested method that should be included in all R6 classes to be passed to
#' methods such as \code{cat}, \code{summary} and \code{print}. Additionally can be used to easily
#' parse R6 objects into data-frames, see examples.
#'
#' @param x R6 object
#' @param ... Additional arguments
#' @usage strprint(x,...)
#'
#' @examples
#' ClassExample <- R6::R6Class("ClassExample",public=list(
#' strprint = function() return("Test"),
#' print = function() print(self$strprint())))
#' ce = ClassExample$new()
#' print(ce)
#' data.frame(A = strprint(ce))
#' strprint(ce)
#' strprint(list(ce,ce))
#'
#' @export
strprint <- function(x,...){
  UseMethod("strprint", x)
}
#' @rdname strprint
#' @export
strprint.R6 <- function(x,...){
  return(x$strprint())
}
#' @rdname strprint
#' @export
strprint.list <- function(x,...){
  return(lapply(x,strprint))
}
testMessage <- function(expr){
  if(inherits(tryCatch(expr, message = function(m) m), "message"))
    return(TRUE)
  else
    return(FALSE)
}
