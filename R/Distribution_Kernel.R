#' @title Abstract Kernel Class
#' @name Kernel
#' @description Abstract class for kernels. Call [listKernels()] for implemented
#' kernels.
#'
#' @section Kernels may include the following methods:
#'
#' * `mode()` - Mode of the kernel, always `0`
#' * `mean()` - Mean of the kernel, always `0`
#' * `median()` - Median of the kernel, always `0`
#' * `skewness()` - Skewness of the kernel, always `0`
#' * `pdfSquared2Norm` - Squared 2-norm of pdf, will be moved to a new decorator
#' * `cdfSquared2Norm` - Squared 2-norm of cdf, will be moved to a new decorator
#'
#' @export
NULL
Kernel <- AbstractClass("Kernel",
  inherit = Distribution,
  lock_objects = FALSE,
  public = list(
    packages = NULL,

    initialize = function(decorators = NULL, support = Interval$new(-1, 1)) {
      assert_pkgload(self$packages)

      if (!is.null(decorators)) suppressMessages(decorate(self, decorators))

      private$.properties$support <- assertSet(support)
      private$.traits$type <- Reals$new()
      private$.parameters <- pset()

      invisible(self)
    },

    mode = function(which = "all") {
      return(0)
    },

    mean = function(...) {
      return(0)
    },

    median = function() {
      return(0)
    },

    pdfSquared2Norm = function(x = 0, upper = Inf) {
      return(NULL)
    },

    cdfSquared2Norm = function(x = 0, upper = Inf) {
      return(NULL)
    },

    skewness = function(...) return(0)
  ),

  private = list(
    .isPdf = 1L,
    .isCdf = 1L,
    .isQuantile = 1L,
    .isRand = 1L,
    .log = TRUE,
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),
    .properties = list(kurtosis = NULL, skewness = 0, symmetric = "symmetric"),
    .rand = function(n) {
      if (!is.null(private$.quantile)) {
        return(self$quantile(runif(n)))
      } else {
        return(NULL)
      }
    }
  )
)
