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
strprint <- function(x,...){
  UseMethod("strprint", x)
}
strprint.R6 <- function(x,...){
  return(x$strprint())
}
strprint.list <- function(x,...){
  return(lapply(x,strprint))
}
testMessage <- function(expr){
  if(inherits(tryCatch(expr, message = function(m) m), "message"))
    return(TRUE)
  else
    return(FALSE)
}
