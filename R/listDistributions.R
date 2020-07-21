#' @title Lists Implemented Distributions
#' @description Lists distr6 distributions in a data.table or a character vector, can be filtered by
#' traits, implemented package, and tags.
#' @param simplify logical. If FALSE (default) returns distributions with traits as a data.table,
#' otherwise returns distribution names as characters.
#' @param filter list to filter distributions by. See examples.
#' @seealso \code{\link{SDistribution}}
#' @return Either a list of characters (if \code{simplify} is TRUE) or a data.table of
#' \code{SDistribution}s and their traits.
#' @examples
#' listDistributions()
#'
#' # Filter list
#' listDistributions(filter = list(VariateForm = "univariate"))
#'
#' # Filter is case-insensitive
#' listDistributions(filter = list(VaLuESupport = "discrete"))
#'
#' # Multiple filters
#' listDistributions(filter = list(VaLuESupport = "discrete", package = "extraDistr"))
#' @export
listDistributions <- function(simplify = FALSE, filter = NULL) {
  distrs <- .distr6$distributions[order(.distr6$distributions$ClassName), ]

  if (!is.null(filter)) {
    names(filter) <- tolower(names(filter))
    if (checkmate::testList(filter)) {
      if (!is.null(filter$variateform)) {
        distrs <- subset(distrs, distrs$VariateForm == filter$variateform)
      }
      if (!is.null(filter$valuesupport)) {
        distrs <- subset(distrs, distrs$ValueSupport == filter$valuesupport)
      }
      if (!is.null(filter$package)) {
        distrs <- subset(distrs, distrs$Package == filter$package)
      }
      if (!is.null(filter$tags)) {
        distrs <- subset(distrs, distrs$Tags == filter$tags)
      }
    }
  }

  if (simplify) {
    return(unlist(distrs$ClassName))
  } else {
    return(data.table::data.table(distrs))
  }
}
