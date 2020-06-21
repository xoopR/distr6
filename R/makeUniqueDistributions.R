#' @title De-Duplicate Distribution Names
#' @description Helper function to lapply over the given distribution list, and make the
#' `short_name`s unique.
#'
#' @details
#' The \code{short_name}s are made unique by suffixing each with a consecutive
#' number so that the names are no longer duplicated.
#'
#' @param distlist list of Distributions.
#'
#' @examples
#' makeUniqueDistributions(list(Binomial$new(), Binomial$new()))
#' @return The list of inputted distributions except with the \code{short_name}s manipulated as
#' necessary to make them unique.
#'
#' @export
makeUniqueDistributions <- function(distlist) {
  assertDistributionList(distlist)
  distlist <- lapply(distlist, function(x) {
    return(x$clone())
  })
  if (any(duplicated(sort(unlist(lapply(distlist, function(x) x$short_name)))))) {
    count <- table(unlist(lapply(distlist, function(x) x$short_name)))
    x <- 1
    for (i in seq_along(distlist)) {
      if (x == as.numeric(count[names(count) %in% distlist[[i]]$short_name])) {
        distlist[[i]]$short_name <- paste0(distlist[[i]]$short_name, x)
        x <- 1
      } else {
        distlist[[i]]$short_name <- paste0(distlist[[i]]$short_name, x)
        x <- x + 1
      }
    }
  }
  names(distlist) <- unlist(sapply(distlist, function(x) {
    return(x$short_name)
  }))
  return(distlist)
}
