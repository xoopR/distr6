
#-------------------------------------------------------------
# Triangular Kernel
#-------------------------------------------------------------
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
#-------------------------------------------------------------
# Uniform Kernel Definition
#-------------------------------------------------------------
TriangularKernel <- R6Class("TriangularKernel", inherit = Kernel, lock_objects = F)
TriangularKernel$set("public", "name", "TriangularKernel")
TriangularKernel$set("public", "short_name", "Tri")
TriangularKernel$set("public", "description", "Triangular Kernel")
TriangularKernel$set("public", "variance", function() {
  return(1 / 6)
})
TriangularKernel$set("public", "squared2Norm", function() {
  return(2 / 3)
})
TriangularKernel$set("public", "initialize", function(decorators = NULL) {
  super$initialize(
    decorators = decorators,
    support = Interval$new(-1, 1)
  )
})
TriangularKernel$set("private", ".pdf", function(x, log = FALSE) {
  C_TriangularKernelPdf(x, log)
})
TriangularKernel$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE) {
  C_TriangularKernelCdf(x, lower.tail, log.p)
})
TriangularKernel$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE) {
  C_TriangularKernelQuantile(x, lower.tail, log.p)
})

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Tri", ClassName = "TriangularKernel", Support = "[-1,1]", Packages = "-"))
