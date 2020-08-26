#' @name mixturiseVector
#' @title Create Mixture Distribution From Multiple Vectors
#' @description Given `m` vector distributions of length `N`, creates a single vector
#' distribution consisting of `n` mixture distributions mixing the `m` vectors.
#' @details Let \eqn{v1 = (D11, D12,...,D1N)} and \eqn{v2 = (D21, D22,...,D2N)} then the
#' `mixturiseVector` function creates the vector distribution \eqn{v3 = (D31, D32, ..., D3N)}
#' where `D3N = m(D1N, D2N, wN)` where `m` is a mixture distribution with weights `wN`.
#' @param vecdists `(list())`\cr
#' List of [VectorDistribution]s, should be of same length and with the non-`distlist'
#' constructor with the same distribution.
#' @param weights `(character(1)|numeric())`\cr
#' Weights passed to [MixtureDistribution]. Default uniform weighting.
#'
#'
#' @examples
#' \dontrun{
#' v1 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 1:2))
#' v2 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 3:4))
#' mv1 <- mixturiseVector(list(v1, v2))
#'
#' # equivalently
#' mv2 <- VectorDistribution$new(list(
#'   MixtureDistribution$new(distribution = "Binomial", params = data.frame(size = c(1, 3))),
#'   MixtureDistribution$new(distribution = "Binomial", params = data.frame(size = c(2, 4)))
#' ))
#'
#' mv1$pdf(1:5)
#' mv2$pdf(1:5)
#' }
#' @export
mixturiseVector <- function(vecdists, weights = "uniform") {

  nr <- nrow(vecdists[[1]]$modelTable)
  dist <- as.character(unlist(vecdists[[1]]$modelTable$Distribution[[1]]))

  sapply(vecdists, function(.x) {
    if (nrow(.x$modelTable) != nr) {
      stop("All vector distributions must be of same length.")
    }
    if (length(unique(as.character(unlist(.x$modelTable$Distribution)))) > 1) {
      stop("Only one class of distribution can be combined at a time.")
    }
    if (unlist(.x$modelTable$Distribution[[1]]) != dist) {
      stop("Distributions in vector must be of same type.")
    }
  })

  mlst <- vector("list", nr)
  for (i in seq_along(mlst)) {
    dlst <- lapply(vecdists, function(.y) {
      .y$parameters()[paste0(
        as.character(unlist(.y$modelTable[i, 2])), "_")]$values(settable = FALSE)
    })

    mlst[[i]] <- MixtureDistribution$new(distribution = dist,
                            params = dlst, weights = weights)
  }

  VectorDistribution$new(mlst)
}
