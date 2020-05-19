
#-------------------------------------------------------------
# Uniform Kernel
#-------------------------------------------------------------
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
#-------------------------------------------------------------
# Uniform Kernel Definition
#-------------------------------------------------------------
UniformKernel <- R6Class("UniformKernel", inherit = Kernel, lock_objects = F)
UniformKernel$set("public", "name", "UniformKernel")
UniformKernel$set("public", "short_name", "Unif")
UniformKernel$set("public", "description", "Uniform Kernel")
UniformKernel$set("public", "variance", function() {
  return(1 / 3)
})
UniformKernel$set("public", "squared2Norm", function() {
  return(0.5)
})
UniformKernel$set("public", "initialize", function(decorators = NULL) {
  super$initialize(
    decorators = decorators,
    support = Interval$new(-1, 1)
  )
})
UniformKernel$set("private", ".pdf", function(x) {
  rep(0.5, length(x))
})
UniformKernel$set("private", ".cdf", function(x) {
  (0.5 * x) + 0.5
})
UniformKernel$set("private", ".quantile", function(p) {
  2 * (p - 0.5)
})

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Unif", ClassName = "UniformKernel", Support = "[-1,1]", Packages = "-"))
