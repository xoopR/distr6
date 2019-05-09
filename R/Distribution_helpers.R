#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions, either all in a data.frame or filtered by chosen
#' traits and/or properties.
#' @param simplify logical.
#' @param traits list of traits to filter distributions by.
#' @examples
#' listDistributions()
#' listDistributions(traits = list(VariateForm = "univariate"))
#' listDistributions(traits = list(ValueSupport = "discrete"))
#' @export
listDistributions <- function(simplify=FALSE, traits=NULL){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "Distribution_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE"]
  y = y[y!="DistributionWrapper"]
  if(simplify)
    return(y)
  else{
    distrs = do.call(rbind,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      x = x$new()
      ShortName = x$short_name
      Type =  getR6Class(x$type())
      ValueSupport = x$valueSupport()
      VariateForm = x$variateForm()
      return(cbind(ShortName, ClassName, Type, ValueSupport, VariateForm))
    }))
    row.names(distrs) = NULL
    if(!is.null(traits)){
      if(checkmate::testList(traits)){
        for(i in 1:length(traits))
          distrs = distrs[distrs[,colnames(distrs) %in% names(traits)[[i]]] == traits[[i]],]
      }
    }
    if("ShortName" %in% rownames(data.frame(distrs))) distrs = t(distrs)
    return(data.frame(distrs))
  }
}

#' @title De-Duplicate Distributions
#' @description From a list of Distributions with the same short_name, suffix each with a consecutive
#' number so that the names are no longer duplicated.
#' @param distlist list of Distributions.
#' @examples
#' makeUniqueDistributions(list(Binomial$new(), Binomial$new()))
#' @export
makeUniqueDistributions <- function(distlist){
  assertDistributionList(distlist)
  distlist = lapply(distlist, function(x) return(x$clone()))
  if(any(duplicated(sort(unlist(lapply(distlist, function(x) x$short_name)))))){
    count = table(unlist(lapply(distlist, function(x) x$short_name)))
    x = 1
    for(i in 1:length(distlist)){
      if(x == as.numeric(count[names(count) %in% distlist[[i]]$short_name])){
        distlist[[i]]$short_name <- paste0(distlist[[i]]$short_name, x)
        x = 1
      } else {
        distlist[[i]]$short_name <- paste0(distlist[[i]]$short_name, x)
        x = x + 1
      }
    }
  }
  names(distlist) = unlist(sapply(distlist, function(x) return(x$short_name)))
  return(distlist)
}

#' @title Kurtosis Type
#' @description Gets the type of (excess) kurtosis#' @usage skewType(skew)
#' @param kurtosis numeric.
#' @examples
#' exkurtosisType(1)
#' @export
exkurtosisType <- function(kurtosis){
  if(kurtosis < 0)
    return("platykurtic")
  else if(kurtosis == 0)
    return("mesokurtic")
  else
    return("leptokurtic")
}

#' @title Skewness Type
#' @description Gets the type of skewness
#' @usage skewType(skew)
#' @param skew numeric.
#' @examples
#' skewType(1)
#' @export
skewType <- function(skew){
  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}