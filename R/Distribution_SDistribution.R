#' @title Abstract Special Distribution Class
#'
#' @template class_abstract
#' @template field_package
#' @template field_packages
#' @template param_decorators
#' @template param_support
#'
#' @export
SDistribution <- R6Class("SDistribution",
  inherit = Distribution,
  public = list(
    package = "This is now deprecated. Use $packages instead.",
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param type `[set6::Set]` \cr
    #' Type of the distribution.
    #' @param symmetry `character(1)` \cr
    #' Distribution symmetry type, default "asymmetric".
    initialize = function(decorators, support, type,
                          symmetry = c("asymmetric", "symmetric")) {

      abstract(self, "SDistribution", "listDistributions()")

      assert_pkgload(self$packages)
      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }
      private$.traits$type <- assertSet(type)
      private$.properties <- list(
        support = assertSet(support),
        symmetry = match.arg(symmetry)
      )

      invisible(self)
    }
  ),

  private = list(
    .log = TRUE,
    .isPdf = 1L,
    .isCdf = 1L,
    .isQuantile = 1L,
    .isRand = 1L
  )
)
