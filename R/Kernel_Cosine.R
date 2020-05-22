
#' @title Cosine Kernel
#'
#' @description Mathematical and statistical functions for the Cosine kernel defined by the pdf,
#' \deqn{f(x) = (\pi/4)cos(x\pi/2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name Cosine
#'
#' @section Constructor: Cosine$new(decorators = NULL)
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

Cosine <- R6Class("Cosine", inherit = Kernel, lock_objects = F,
  public = list(
    name = "Cosine",
    short_name = "Cos",
    description = "Cosine Kernel",

    squared2Norm = function() {
      return(pi^2 / 16)
    },
    variance = function() {
      return(1 - 8 / (pi^2))
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_CosineKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_CosineKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_CosineKernelQuantile(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Cos", ClassName = "Cosine", Support = "[-1,1]", Packages = "-"))
