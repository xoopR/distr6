#' @title Uniform Kernel
#'
#' @description Mathematical and statistical functions for the Uniform kernel defined by the pdf,
#' \deqn{f(x) = 1/2}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name UniformKernel
#'
#' @section Constructor: UniformKernel$new(decorators = NULL)
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

UniformKernel <- R6Class("UniformKernel", inherit = Kernel, lock_objects = F,
  public = list(
    name = "UniformKernel",
    short_name = "Unif",
    description = "Uniform Kernel",

    squared2Norm = function() {
      return(0.5)
    },
    variance = function() {
      return(1 / 3)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_UniformKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_UniformKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_UniformKernelQuantile(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Unif", ClassName = "UniformKernel", Support = "[-1,1]", Packages = "-"))
