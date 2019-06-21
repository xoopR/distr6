#' @title Lists Implemented Distribution Decorators
#' @description Lists decorators that can decorate an R6 Distribution.
#' @param simplify logical. If TRUE (default) returns results as characters.
#' @param get logical. If TRUE (default) gets results as R6 classes, ignored if simplify = TRUE.
#' @details If \code{simplify} and \code{get} are both false then returns a data.table of decorator
#' names and methods added by the decorator.
#' @examples
#' listDecorators()
#' listDecorators(FALSE)
#' @export
listDecorators <- function(simplify = TRUE, get = TRUE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "DistributionDecorator_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE"]
  if(simplify)
    return(as.character(y))
  else if(get)
    return(lapply(y, get))
  else{
    decorators = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      Name = x$classname
      Methods = paste0(names(x$public_methods)[2:length(x$public_methods)],
                       collapse = "; ")
      return(cbind(Name, Methods))
    }))
    row.names(decorators) = NULL
    return(data.table::data.table(decorators))
  }
}
