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
        if (abs(x[i]) > 2) {
          ret[i] = 0
        } else if (x[i] >= 0) {
          if (upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] = (35 / 1757184) * (20 * (1 + upper[i])^7 *
                     (1024 + 7 * upper[i] * (-595 + upper[i] * (1093 + 3 * upper[i] * (-378 + upper[i] * (230 + 11 * (-7 + upper[i]) * upper[i]))))) -
                     30030 * (-1 + upper[i]^2)^6 * x[i] +
                     156 * (-256 - 1155 * upper[i] + 3465 * upper[i]^3 - 6006 * upper[i]^5 + 5610 * upper[i]^7 - 2695 * upper[i]^9 + 525 * upper[i]^11) * x[i]^2 -
                     60060 * (-1 + upper[i]^2)^4 * (-1 + 2 * (upper[i]^2)) * (x[i]^3) +
                     572 * (64 + 315 * upper[i] - 840 * upper[i]^3 + 1134 * upper[i]^5 - 720 * upper[i]^7 + 175 * upper[i]^9) * (x[i]^4) -
                     45045 * (-1 + upper[i]^2)^4 * (x[i]^5) + 1716 * (1 + upper[i])^4 * (-16 + upper[i] *(29 + 5 * (-4 + upper[i]) * upper[i])) * (x[i]^6) +
                     27456 * (x[i]^7) - 2288 * (x[i]^9) + 156 * (x[i]^11) - 5 * (x[i]^13))
          } else if (upper[i] >= 1 | upper[i] == Inf){
            ret[i] = 350 / 429 - (35 * x[i]^2) / 22 + (35*  x[i]^4) / 24 - (35 *  x[i]^6) / 32 + (35 * x[i]^7) / 64 -
                      (35 * x[i]^9) / 768 + (35 * x[i]^11) / 11264 - (175 * x[i]^13) / 1757184
          }
        } else if (x[i] <= 0) {
          if (upper[i] <= -1) {
            ret[i] = 0
          } else if (upper[i] <= x[i] + 1 & upper[i] >= -1) {
            ret[i] = (35 / 1757184) * (20 * (1 + upper[i])^7 *
                     (1024 + 7 * upper[i] * (-595 + upper[i] * (1093 + 3 * upper[i] * (-378 + upper[i] * (230 + 11 * (-7 + upper[i]) * upper[i]))))) -
                      30030 * (-1 + upper[i]^2)^6 * x[i] + 156 * (-256 - 1155 * upper[i] + 3465 * upper[i]^3  -
                      6006 * upper[i]^5 + 5610 *upper[i]^7 - 2695 * upper[i]^9 + 525 * upper[i]^11) * x[i]^2   -
                      60060 * (-1 + upper[i]^2)^4 * (-1 + 2 * upper[i]^2) * x[i]^3 +
                      572 * (64 + 315 * upper[i] - 840 * upper[i]^3 + 1134 * upper[i]^5 - 720 * upper[i]^7 + 175 * upper[i]^9) * x[i]^4 -
                      45045 * (-1 + upper[i]^2)^4 * x[i]^5 + 1716 * (1 + upper[i])^4 * (-16 + upper[i] * (29 + 5 * (-4 + upper[i]) * upper[i])) * x[i]^6)
          } else if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = 350 / 429 - (35 * x[i]^2) / 22 + (35 * x[i]^4) / 24 - (35 * x[i]^6) / 32 - (35 * x[i]^7) / 64 +
                      (35 * x[i]^9) / 768 - (35 * x[i]^11) / 11264 + (175 * x[i]^13) / 1757184
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
