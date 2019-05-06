#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions, either all in a data.frame or filtered by chosen
#' traits and/or properties.
#' @param simplify logical.
#' @param traits list of traits to filter distributions by.
#' @param properties list of traits to filter distributions by.
#' @examples
#' listDistributions()
#' listDistributions(traits = list(variateForm = "univariate"))
#' listDistributions(properties = list(symmetry = FALSE))
listDistributions <- function(simplify=FALSE, traits=NULL, properties = NULL){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "Distribution_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = (y[y!="FALSE"])
  if(simplify)
    return(y)
  else{
    distrs = do.call(rbind,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      x = x$new()
      ShortName = x$short.name
      traits = as.data.frame(x$getTraits())
      return(cbind(ShortName,ClassName,traits))
    }))
    row.names(distrs) = NULL
    if(!is.null(traits)){
      traits = as.list(traits)
      for(i in 1:length(traits))
        distrs = distrs[distrs[,colnames(distrs) %in% names(traits)[[i]]] == traits[[i]],]
    }
    return(distrs)
  }
}

#' @title Kurtosis Type
#' @description Gets the type of (excess) kurtosis#' @usage skewType(skew)
#' @param kurtosis numeric.
#' @examples
#' exkurtosisType(1)
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
skewType <- function(skew){
  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}