#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions, either all in a data.frame or filtered by chosen
#' traits and/or properties.
#' @param simplify logical.
#' @param traits list of traits to filter distributions by.
#' @param view logical, if TRUE displays Distributions in Viewer. Ignored if \code{simplify} is FALSE.
#' @examples
#' listKernels()
#' listKernels(traits = list(VariateForm = "univariate"))
#' listKernels(traits = list(ValueSupport = "discrete"))
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
    distrs = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      ShortName = x$public_fields$short_name
      Support = x$new()$support()$getSymbol()
      return(cbind(ShortName, ClassName, Support))
    }))
    row.names(distrs) = NULL

    return(data.frame(distrs))
  }
}
