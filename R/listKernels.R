#' @title Lists Implemented Kernels
#' @description Lists all implemented kernels in distr6.
#' @param simplify logical. If FALSE (default) returns kernels with support as a data.table, otherwise returns
#' kernel names as characters.
#' @seealso \code{\link{Kernel}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of \code{Kernel}s and their traits.
#' @examples
#' listKernels()
#'
#' @export
listKernels <- function(simplify=FALSE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "Kernel_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE"]
  if(simplify)
    return(as.character(y))
  else{
    distrs = do.call(rbind.data.frame,c(lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      ShortName = x$public_fields$short_name
      Support = x$new()$support()$getSymbol()
      return(cbind(ShortName, ClassName, Support))
    }), list(stringsAsFactors = FALSE)))
    row.names(distrs) = NULL

    return(data.table::data.table(distrs, stringsAsFactors = FALSE))
  }
}
