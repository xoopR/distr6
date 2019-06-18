#' @title Lists Implemented R6 Distributions
#' @description Lists R6 distributions, either all in a data.frame or filtered by chosen
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
    distrs = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      ShortName = x$public_fields$short_name
      Type = x$public_fields$traits$type$getSymbol()
      ValueSupport = x$public_fields$traits$valueSupport
      VariateForm = x$public_fields$traits$variateForm
      return(cbind(ShortName, ClassName, Type, ValueSupport, VariateForm))
    }))
    row.names(distrs) = NULL
    if(!is.null(traits)){
      names(traits) = tolower(names(traits))
      if(checkmate::testList(traits)){
        if(is.null(traits$valuesupport) & !is.null(traits$variateform))
          distrs = dplyr::filter(distrs, distrs$VariateForm == traits$variateform)
        else if(is.null(traits$variateform) & !is.null(traits$valuesupport))
          distrs = dplyr::filter(distrs, distrs$ValueSupport == traits$valuesupport)
        else if(!is.null(traits$variateform) & !is.null(traits$valuesupport))
          distrs = dplyr::filter(distrs, distrs$VariateForm == traits$variateform & distrs$ValueSupport == traits$valuesupport)
      }
    }
    if("ShortName" %in% rownames(data.frame(distrs))) distrs = t(distrs)
    if(view)
      utils::View(data.frame(distrs))
    else
      return(data.frame(distrs))
  }
}

#' @title Lists Implemented Distribution Decorators
#' @description Lists decorators that can decorate an R6 Distribution.
#' @param simplify logical. If TRUE (default) returns results as characters, otherwise R6 classes.
#' @examples
#' listDecorators()
#' listDecorators(FALSE)
#' @export
listDecorators <- function(simplify = TRUE){
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
  else
    return(lapply(y, get))
}

#' @title Lists Implemented Distribution Decorators
#' @description Lists wrappers that can wrap an R6 Distribution.
#' @param simplify logical. If TRUE (default) returns results as characters, otherwise R6 classes.
#' @examples
#' listWrappers()
#' listWrappers(TRUE)
#' @export
listWrappers <- function(simplify = TRUE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "DistributionWrapper_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = y[y!="FALSE" & y!="ConcreteWrapper"]
  if(simplify)
    return(as.character(y))
  else
    return(lapply(y, get))
}

#' @title Lists Implemented R6 Special Sets
#' @description Lists special sets that can be used in SetInterval.
#' @param simplify logical. If FALSE (default) returns data.frame of set name and symbol, otherwise character.
#' @param view logical, if TRUE displays Distributions in Viewer. Ignored if \code{simplify} is FALSE.
#' @examples
#' listSpecialSets()
#' listSpecialSets(TRUE)
#' @export
listSpecialSets <- function(simplify = FALSE, view = FALSE){
  y = sapply(ls(name="package:distr6"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "SpecialSet_generator" |
         environmentName(get(x)$get_inherit()) == "Rationals_generator" |
         environmentName(get(x)$get_inherit()) == "Reals_generator")
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
    symbols = do.call(rbind.data.frame,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      x = x$new()
      Symbol = x$getSymbol()
      Lower = x$lower()
      Upper = x$upper()
      return(cbind(ClassName, Symbol, Lower, Upper))
    }))
    row.names(symbols) = NULL

    if(view)
      utils::View(data.frame(symbols))
    else
      return(symbols)
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
#' @description Gets the type of (excess) kurtosis
#' @param kurtosis numeric.
#' @examples
#' exkurtosisType(1)
#' @export
exkurtosisType <- function(kurtosis){

  if(is.nan(kurtosis)) return("undefined")

  if(kurtosis < 0)
    return("platykurtic")
  else if(kurtosis == 0)
    return("mesokurtic")
  else
    return("leptokurtic")
}

#' @title Skewness Type
#' @description Gets the type of skewness
#' @param skew numeric.
#' @examples
#' skewType(1)
#' @export
skewType <- function(skew){

  if(is.nan(skew)) return("undefined")

  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}

#' @title Generalised P-Norm
#' @description Calculate the p-norm of any function between given limits. Given by,
#' \deqn{(\int_S |f|^p d\mu)^1/p}
#' @usage generalPNorm(fun, p, lower, upper)
#' @param fun function to calculate the p-norm of.
#' @param p the pth norm to calculate
#' @param lower lower bound for the integral
#' @param upper upper bounde for the integral
#'
#' @examples
#' generalPNorm(Exponential$new()$pdf,2,0,10)
#'
#' @export
generalPNorm <- function(fun, p, lower, upper){
  warning(.distr6$message_numeric)
  return((stats::integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
}
