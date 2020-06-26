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
    pdfSquared2Norm = function(x = 0, upper = Inf) {
      ret <- numeric(length(x))
      for (i in seq_along(x)) {
        if (upper[i] == Inf) {
          if (abs(x[i]) > 2) {
            ret[i] = 0
          } else {
            ret[i] = 35 * (-5 * abs(x[i])^13 + 156 * abs(x[i])^11 - 2288 * abs(x[i])^9 +
                             27456 * abs(x[i])^7 - 54912 * abs(x[i])^6 + 73216 * abs(x[i])^4 -
                             79872 * abs(x[i])^2 + 40960) / 1757184
          }
        }
      }
      return(ret)
    },

    #' @description
    #' The squared 2-norm of the cdf is defined by
    #' \deqn{\int_a^b (F_X(u))^2 du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    cdfSquared2Norm = function(x = 0, upper = Inf) {

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

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Triw", ClassName = "Triweight",
    Support = "[-1,1]", Packages = "-"
  )
)
