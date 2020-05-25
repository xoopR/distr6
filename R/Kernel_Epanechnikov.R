#' @title Epanechnikov Kernel
#'
#' @description Mathematical and statistical functions for the Epanechnikov kernel defined by the pdf,
#' \deqn{f(x) = \frac{3}{4}(1-x^2)}{f(x) = 3/4(1-x^2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The quantile function is omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Epanechnikov
#' @template class_distribution
#' @template class_kernel
#'
#' @export
Epanechnikov <- R6Class("Epanechnikov", inherit = Kernel, lock_objects = F,
  public = list(
    name = "Epanechnikov",
    short_name = "Epan",
    description = "Epanechnikov Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    squared2Norm = function() {
      return(3 / 5)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(1 / 5)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_EpanechnikovKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_EpanechnikovKernelCdf(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Epan", ClassName = "Epanechnikov", Support = "[-1,1]", Packages = "-"))
