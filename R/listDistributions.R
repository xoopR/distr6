#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions, either all in a data.table or filtered by chosen
#' traits and/or properties.
#' @param simplify logical.
#' @param traits list of traits to filter distributions by.
#' @param view logical, if TRUE displays Distributions in Viewer. Ignored if \code{simplify} is FALSE.
#' @examples
#' listDistributions()
#' listDistributions(traits = list(VariateForm = "univariate"))
#' listDistributions(traits = list(ValueSupport = "discrete"))
#' @export
listDistributions <- function(simplify=FALSE, traits=NULL, view = FALSE){
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
    if(!is.null(traits)){
      names(traits) = tolower(names(traits))
      if(checkmate::testList(traits)){
        if(!is.null(traits$variateform))
          distrs = subset(distrs, distrs$VariateForm == traits$variateform)
        if(!is.null(traits$valuesupport))
          distrs = subset(distrs, distrs$ValueSupport == traits$valuesupport)
        if(!is.null(traits$package))
          distrs = subset(distrs, distrs$Package == traits$package)
      }
    }
    if("ShortName" %in% rownames(data.frame(distrs))) distrs = t(distrs)
    if(view)
      utils::View(data.table::data.table(distrs))
    else
      return(data.table::data.table(distrs))
  }
}
