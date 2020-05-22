
#' @title Triangular Kernel
#'
#' @description Mathematical and statistical functions for the Triangular kernel defined by the pdf,
#' \deqn{f(x) = 1 - |x|}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name TriangularKernel
#'
#' @section Constructor: TriangularKernel$new(decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' }
#'
#' @inheritSection Kernel Public Variables
#' @inheritSection Kernel Public Methods
#'
#' @return Returns an R6 object inheriting from class Kernel.
#'
#' @export
NULL

TriangularKernel <- R6Class("TriangularKernel", inherit = Kernel, lock_objects = F,
  public = list(
    name = "TriangularKernel",
    short_name = "Tri",
    description = "Triangular Kernel",

    squared2Norm = function() {
      return(2 / 3)
    },
    variance = function() {
      return(1 / 6)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TriangularKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_TriangularKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_TriangularKernelQuantile(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Tri", ClassName = "TriangularKernel", Support = "[-1,1]", Packages = "-"))
