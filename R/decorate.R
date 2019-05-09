#' @export
decorate <- function(distribution, decorators){
  if(!checkmate::testList(decorators))
    decorators = list(decorators)
  dist_decors = distribution$decorators
  decors_names = lapply(decorators, function(x) x$classname)
  decorators = decorators[!(decors_names %in% dist_decors)]

  dist_name = substitute(distribution)

  if(length(decorators) == 0)
    return(paste(substitute(distribution),"is already decorated with",
                  paste0(decors_names,collapse=",")))
  else{
    lapply(decorators, function(a_decorator){
      methods <- c(a_decorator$public_methods, get(paste0(a_decorator$inherit))$public_methods)
      methods <- methods[!(names(methods) %in% c("initialize","clone"))]
      methods <- methods[!(names(methods) %in% ls(distribution))]

      for(i in 1:length(methods)){
        formals(methods[[i]]) = c(formals(methods[[i]]),list(self=distribution))
        assign(names(methods)[[i]],methods[[i]],envir=as.environment(distribution))
      }
    })

    unlockBinding("decorators", distribution)
    distribution$decorators = unlist(decors_names)
    lockBinding("decorators", distribution)

    message(paste(dist_name,"is now decorated with",
                  paste0(decors_names,collapse = ",")))
  }
}