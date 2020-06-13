#' @title Triangular Kernel
#'
#' @description Mathematical and statistical functions for the Triangular kernel defined by the pdf,
#' \deqn{f(x) = 1 - |x|}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name TriangularKernel
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#'
#' @export
TriangularKernel <- R6Class("TriangularKernel",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "TriangularKernel",
    short_name = "Tri",
    description = "Triangular Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0) {
      return(2 / 3)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
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
