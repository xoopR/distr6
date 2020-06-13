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
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Triweight <- R6Class("Triweight",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Triweight",
    short_name = "Triw",
    description = "Triweight Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0) {
      return(350 / 429)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(1 / 9)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TriweightKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_TriweightKernelCdf(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Triw", ClassName = "Triweight", Support = "[-1,1]", Packages = "-"))
