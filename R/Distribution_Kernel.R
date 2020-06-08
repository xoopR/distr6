#' @title Abstract Kernel Class
#'
#' @template class_abstract
#' @template field_package
#' @template field_packages
#' @template param_decorators
#' @template param_support
#' @template method_mode
#'
#' @export
Kernel <- R6Class("Kernel",
  inherit = Distribution,
  public = list(
    package = "This is now deprecated. Use $packages instead.",
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(decorators = NULL, support = Interval$new(-1, 1)) {
      if (getR6Class(self) == "Kernel") {
        stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Use listKernels() to see the kernels currently implemented in distr6, or Distribution$new() to construct a custom Kernel."))
      }

      assert_pkgload(self$packages)

      if (!is.null(decorators)) suppressMessages(decorate(self, decorators))

      private$.properties$support <- assertSet(support)
      private$.traits$type <- Reals$new()

      invisible(self)
    },

    #' @description
    #' Calculates the mode of the distibution.
    mode = function(which = "all") {
      return(0)
    },

    #' @description
    #' Calculates the mean (expectation) of the distribution.
    mean = function() {
      return(0)
    },

    #' @description
    #' Calculates the median of the distribution.
    median = function() {
      return(0)
    }
  ),

  private = list(
    .log = TRUE,
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),
    .properties = list(kurtosis = NULL, skewness = NULL, symmetric = "symmetric"),
    .rand = function(n) {
      if (!is.null(private$.quantile)) {
        return(self$quantile(runif(n)))
      } else {
        return(NULL)
      }
    }
  )
)
