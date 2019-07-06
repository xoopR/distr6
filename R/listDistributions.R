#' @title Lists Implemented Distributions
#' @description Lists distr6 distributions in a data.table or a character vector, can be filtered by
#' traits and implemented package.
#' @param simplify logical. If FALSE (default) returns distributions with traits as a data.table, otherwise returns
#' distribution names as characters.
#' @param filter list to filter distributions by. See examples.
#' @seealso \code{\link{SDistribution}}
#' @examples
#' listDistributions()
#'
#' # Filter list
#' listDistributions(filter = list(VariateForm = "univariate"))
#'
#' # Filter is case-insensitive
#' listDistributions(filter = list(VaLuESupport = "discrete"))
#'
#' # Multiple filters
#' listDistributions(filter = list(VaLuESupport = "discrete", package = "distr6"))
#'
#' @export
listDistributions <- function(simplify=FALSE, filter=NULL){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "SDistribution_generator")
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
    distrs = data.table::rbindlist(lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      ShortName = x$public_fields$short_name
      Package = x$public_fields$package
      x = x$new()
      ValueSupport = x$valueSupport()
      VariateForm = x$variateForm()
      Type = x$type()$getSymbol()
      return(data.table::data.table(ShortName, ClassName, Type, ValueSupport, VariateForm, Package))
    }))
    row.names(distrs) = NULL
    if(!is.null(filter)){
      names(filter) = tolower(names(filter))
      if(checkmate::testList(filter)){
        if(!is.null(filter$variateform))
          distrs = subset(distrs, distrs$VariateForm == filter$variateform)
        if(!is.null(filter$valuesupport))
          distrs = subset(distrs, distrs$ValueSupport == filter$valuesupport)
        if(!is.null(filter$package))
          distrs = subset(distrs, distrs$Package == filter$package)
      }
    }

    return(data.table::data.table(distrs))
  }
}
