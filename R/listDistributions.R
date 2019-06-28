#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions in a data.table or a chaarcter vector, can be filtered by
#' traits and implemented package.
#' @param simplify logical.
#' @param filter list to filter distributions by.
#' @param view logical, if TRUE displays Distributions in Viewer. Ignored if \code{simplify} is FALSE.
#' @examples
#' listDistributions()
#' listDistributions(filter = list(VariateForm = "univariate"))
#' listDistributions(filter = list(ValueSupport = "discrete"))
#' @export
listDistributions <- function(simplify=FALSE, filter=NULL, view = FALSE){
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
      Type = x$public_fields$traits$type$getSymbol()
      ValueSupport = x$public_fields$traits$valueSupport
      VariateForm = x$public_fields$traits$variateForm
      Package = x$public_fields$package
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
    if("ShortName" %in% rownames(data.frame(distrs))) distrs = t(distrs)
    if(view)
      utils::View(data.table::data.table(distrs))
    else
      return(data.table::data.table(distrs))
  }
}
