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
