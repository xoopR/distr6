
#-------------------------------------------------------------
# Triweight Kernel
#-------------------------------------------------------------
#' @title Triweight Kernel
#'
#' @description Mathematical and statistical functions for the Triweight kernel defined by the pdf,
#' \deqn{f(x) = 35/32(1 - x^2)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The quantile function is omitted as no closed form analytic expression could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Triweight
#'
#' @section Constructor: Triweight$new(decorators = NULL)
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
#-------------------------------------------------------------
# Triweight Kernel Definition
#-------------------------------------------------------------
Triweight <- R6Class("Triweight", inherit = Kernel, lock_objects = F)
Triweight$set("public", "name", "Triweight")
Triweight$set("public", "short_name", "Triw")
Triweight$set("public", "description", "Triweight Kernel")
Triweight$set("public", "squared2Norm", function() {
  return(350 / 429)
})
Triweight$set("public", "variance", function() {
  return(1 / 9)
})
Triweight$set("public", "initialize", function(decorators = NULL) {
  super$initialize(
    decorators = decorators,
    support = Interval$new(-1, 1)
  )
})
Triweight$set("private", ".pdf", function(x) {
  35 / 32 * (1 - x^2)^3
})
Triweight$set("private", ".cdf", function(x) {
  35 / 32 * (x - x^3 + 3 / 5 * x^5 - 1 / 7 * x^7 + 16 / 35)
})

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Triw", ClassName = "Triweight", Support = "[-1,1]", Packages = "-"))
