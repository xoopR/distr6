
#' @title Tricube Kernel
#'
#' @description Mathematical and statistical functions for the Tricube kernel defined by the pdf,
#' \deqn{f(x) = 70/81(1 - |x|^3)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Tricube
#'
#' @section Constructor: Tricube$new(decorators = NULL)
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

Tricube <- R6Class("Tricube", inherit = Kernel, lock_objects = F,
  public = list(
    name = "Tricube",
    short_name = "Tric",
    description = "Tricube Kernel",

    squared2Norm = function() {
      return(175 / 247)
    },
    variance = function() {
      return(35 / 243)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TricubeKernelPdf(x, log)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Tric", ClassName = "Tricube", Support = "[-1,1]", Packages = "-"))
