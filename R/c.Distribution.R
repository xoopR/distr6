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

  classes <- vapply(distlist, getR6Class, character(1))

  if (length(distribution <- unique(classes)) == 1) {
    if (distribution != "VectorDistribution") {
      params <- lapply(distlist, function(x) x$parameters()$values)
      return(VectorDistribution$new(
        distribution = distribution, params = params, name = name,
        short_name = short_name, decorators = decorators
      ))
    } else {
      ## if there are any distlist VDs then forced to construct the same
      if (any(vapply(distlist, function(x) x$distlist, logical(1)))) {
        return(VectorDistribution$new(
          unlist(lapply(distlist, function(x) x$wrappedModels())),
          name = name, short_name = short_name, decorators = decorators
        ))
      } else {
        distribution <- unlist(lapply(distlist, function(x) {
          as.character(unlist(x$modelTable$Distribution))
        }))
        if (length(unique(distribution)) == 1) {
          ## if all distributions same can construct with distr/params
          return(VectorDistribution$new(
            vecdist = distlist, name = name,
            short_name = short_name,
            decorators = decorators
          ))
        } else {
          ## otherwise forced to use distlist
          return(VectorDistribution$new(
            unlist(lapply(distlist, function(x) x$wrappedModels())),
            name = name, short_name = short_name, decorators = decorators
          ))
        }
      }
    }
  }

  # If any are VectorDistributions then get the wrapped list
  if ("VectorDistribution" %in% classes) {
    distlist <- unlist(lapply(distlist, function(x) {
      if (getR6Class(x) == "VectorDistribution") {
        list(x$wrappedModels())
      } else {
        list(x)
      }
    }))
  }


  VectorDistribution$new(distlist, name = name, short_name = short_name,
                         decorators = decorators)
}
