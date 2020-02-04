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

makeChecks <- function(assertionName, cond, defaulterrormsg, args = alist(object=,errormsg=),
                       pos = -1){
  cond = substitute(cond)
  value = function(){}
  args$errormsg = substitute(defaulterrormsg)
  formals(value) = args
  body(value) = substitute(assertThat(object,arg1,errormsg),list(arg1=cond))
  assign(paste0("assert",assertionName), value = value,
         pos = pos)

  body(value) = substitute(checkThat(arg1,errormsg),list(arg1=cond))
  assign(paste0("check",assertionName), value = value,
         pos = pos)

  body(value) = substitute(testThat(arg1),list(arg1=cond))
  assign(paste0("test",assertionName), value = value,
         pos = pos)
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
    if(missing(noerror))
      return(x)
    else
      return(noerror)
  }
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
testMessage <- function(expr){
  if(inherits(tryCatch(expr, message = function(m) m), "message"))
    return(TRUE)
  else
    return(FALSE)
}

modal = function(data){
  tab = table(unlist(data))
  modal = as.numeric(names(tab)[tab==max(tab)])
  return(modal)
}

makeUniqueNames <- function(y){
  if (any(duplicated(sort(y)))) {
    count = table(y)
    x = 1
    for(i in 1:length(y)){
      if(x == as.numeric(count[names(count) %in% y[[i]]])){
        y[[i]] <- paste0(y[[i]], x)
        x = 1
      } else {
        y[[i]] <- paste0(y[[i]], x)
        x = x + 1
      }
    }
  }
  return(y)
}

#' @title Coerce String to Proper Case
#' @description Helper function for string coercion to proper case
#' @param str String to coerce
#' @param split see \code{\link[base]{strsplit}}
#' @param fixed see \code{\link[base]{strsplit}}
#' @export
toproper <- function(str, split = " ", fixed = TRUE){
  str = strsplit(str, split, fixed)
  str = lapply(str, function(x){
    paste0(toupper(substr(x,1,1)), tolower(substr(x,2,1000)), collapse = split)
  })
  return(unlist(str))
}

assert_pkgload <- function(pkgs) {
  if(!is.null(pkgs)){
    check = sapply(pkgs, requireNamespace, quietly = TRUE)
    if(!all(check)) {
      stop(sprintf("The following packages could not be loaded, please install: %s",
                   paste0("{", paste0(pkgs[!check], collapse = ","), "}")))
    }
  }
}
