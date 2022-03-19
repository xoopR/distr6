#' @title Mix Matrix Distributions into a new Matdist
#' @description Given `m` matrix distributions distributions of length `N`,
#' creates a new Matdist by summing over the weighted cdfs. Note that this
#' method does *not* create a [MixtureDistribution] but a new [Matdist].
#' Assumes Matrix distributions have the same number of columns, otherwise use
#' `mixturiseVector(lapply(mds, as.VectorDistribution))`.
#' @details
#' This method returns a new [Matdist] which is less flexible than a
#' [MixtureDistribution] which has parameters (i.e. `weights`) that can be
#' updated after construction.
#'
#' @param mds `(list())`\cr
#' List of [Matdist]s, should have same number of rows and columns.
#' @param weights `(character(1)|numeric())`\cr
#' Individual distribution weights. Default uniform weighting (`"uniform"`).
#'
#' @seealso
#' [mixturiseVector]
#'
#' @examples
#' m1 <- as.Distribution(
#'  t(apply(matrix(runif(25), 5, 5, FALSE,
#'                  list(NULL, 1:5)), 1,
#'          function(x) x / sum(x))),
#'  fun = "pdf"
#')
#' m2 <- as.Distribution(
#'  t(apply(matrix(runif(25), 5, 5, FALSE,
#'                  list(NULL, 1:5)), 1,
#'          function(x) x / sum(x))),
#'  fun = "pdf"
#')
#' # uniform mixing
#' m3 <- mixMatrix(list(m1, m2))
#'
#' # un-uniform mixing
#' m4 <- mixMatrix(list(m1, m2), weights = c(0.1, 0.9))
#'
#' m1$cdf(3)
#' m2$cdf(3)
#' m3$cdf(3)
#' m4$cdf(3)
#'
#' @export
mixMatrix <- function(mds, weights = "uniform") {
  # get weights
  if (is.character(weights) && weights == "uniform") {
    weights <- rep(1 / length(mds), length(mds))
  }

  pdfs <- .merge_matpdf_cols(lapply(mds, gprm, "pdf"))

  # check mds compatible
  if (length(unique(viapply(pdfs, nrow))) > 1) {
    stop("Can't mix distributions with different number of rows")
  }

  # mix mds
  out <- matrix(0, nrow(pdfs[[1]]), ncol(pdfs[[1]]))
  for (i in seq_along(pdfs)) {
    out <- out + (pdfs[[i]] * weights[[i]])
  }

  # convert to new Matdist
  dstr("Matdist", pdf = out)
}
