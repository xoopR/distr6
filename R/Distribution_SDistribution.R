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

      # check required packages are loaded
      assert_pkgload(self$packages)

      # decorate if requested
      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }

      private$.traits$type <- assertSet(type)
      private$.properties <- list(
        support = assertSet(support),
        symmetry = match.arg(symmetry)
      )

      if (!grepl("Empirical", getR6Class(self))) {
        args <- getR6Call()
        private$.parameters <- do.call(getParameterSet,
                                        c(list(object = self), args))
        if (length(args)) {
          linked <- private$.parameters$tag_properties$linked
          if (length(linked)) {
            vals <- unlist(lapply(linked, function(.x) {
              which <- names(private$.parameters$get_values(tags = .x, simplify = FALSE))
              if (any(names(args) %in% which)) {
                expand_list(which, args[names(args) %in% which])
              } else {
                private$.parameters$get_values(tags = .x, simplify = FALSE)[1]
              }
            }), recursive = FALSE)

            self$set_values(lst = vals)
          } else {
            self$set_values(lst = args)
          }
        }
      }

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
