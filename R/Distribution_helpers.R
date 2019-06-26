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
#' @param upper upper bound for the integral
#'
#' @examples
#' generalPNorm(Exponential$new()$pdf,2,0,10)
#'
#' @export
generalPNorm <- function(fun, p, lower, upper){
  warning(.distr6$message_numeric)
  return((stats::integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
}
