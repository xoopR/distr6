#' @title Combine Distributions into a VectorDistribution
#' @description Helper function for quickly combining distributions into a [VectorDistribution].
#' @param ... distributions to be concatenated.
#' @param name,short_name,decorators See \code{\link{VectorDistribution}}
#' @return A VectorDistribution
#' @seealso \code{\link{VectorDistribution}}
#' @examples
#' # Construct and combine
#' c(Binomial$new(), Normal$new())
#'
#' # More complicated distributions
#' b <- truncate(Binomial$new(), 2, 6)
#' n <- huberize(Normal$new(), -1, 1)
#' c(b, n)
#'
#' # Concatenate VectorDistributions
#' v1 <- VectorDistribution$new(list(Binomial$new(), Normal$new()))
#' v2 <- VectorDistribution$new(
#'   distribution = "Gamma",
#'   params = data.table::data.table(shape = 1:2, rate = 1:2)
#' )
#' c(v1, v2)
#' @export
c.Distribution <- function(..., name = NULL, short_name = NULL, decorators = NULL) {
  # Get list of inputs and assert all distributions
  distlist <- list(...)
  assertDistributionList(distlist)

  # If all distributions in the list are VectorDistributions then try and return a
  # VectorDistribution with distribution/params constructor.
  if (all(sapply(distlist, getR6Class) %in% "VectorDistribution")) {
    if (any(sapply(distlist, function(x) x$distlist))) {
      return(VectorDistribution$new(unlist(lapply(distlist, function(x) x$wrappedModels()))))
    } else {
      distribution <- unlist(lapply(distlist, function(x) {
        as.character(unlist(x$modelTable$Distribution))
      }))
      if (length(unique(distribution)) == 1) {
        return(VectorDistribution$new(vecdist = distlist))
      } else {
        return(VectorDistribution$new(unlist(lapply(distlist, function(x) x$wrappedModels()))))
      }
    }
  }

  # If any are VectorDistributions then get the wrapped list
  distlist <- unlist(lapply(distlist, function(x) {
    ifelse(getR6Class(x) == "VectorDistribution", list(x$wrappedModels()), list(x))
  }))

  # Create VectorDistribution
  return(VectorDistribution$new(distlist,
    name = name, short_name = short_name,
    decorators = decorators
  ))
}
