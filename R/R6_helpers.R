strprint.list <- function(x,...){
  lapply(x,strprint,...)
}

getR6Class <- function(x){
  return(get(class(x)[[1]])$classname)
}