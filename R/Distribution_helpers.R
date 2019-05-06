listDistributions <- function(simplify=FALSE,traits=NULL){
  y = sapply(ls(name=".GlobalEnv"),function(x){
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

liesInType <- function(distribution, x){
  if(x <= distribution$type()$sup() & x >= distribution$type()$inf())
    return(TRUE)
  else
    return(FALSE)
}


exkurtosisType <- function(kurtosis){
  if(kurtosis < 0)
    return("platykurtic")
  else if(kurtosis == 0)
    return("mesokurtic")
  else
    return("leptokurtic")
}
skewType <- function(skew){
  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}