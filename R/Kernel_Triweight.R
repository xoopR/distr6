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
        if (abs(x[i]) >= 2) {
          ret[i] = 0
        } else if (x[i] >= 0 & x[i] <= 2) {
          if (upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] = (35 / 32)^(2) * (-x[i]^13 / 12012 + x[i]^11  /385 - (4 * x[i]^9) / 105 + (16 * x[i]^7) / 35 +
                                         upper[i]^3 * (x[i]^6 - 8 * x[i]^4 + 9 * x[i]^2 - 2) - (16 * x[i]^6) / 35 +
                                         upper[i]^7 * (x[i]^6 / 7 - (48 * x[i]^4) / 7 + (102 * x[i]^2) / 7 - 20 / 7) +
                                         upper[i]^5 * (- (3 * x[i]^6) / 5 + (54 * x[i]^4) / 5 - (78 * x[i]^2) / 5 + 3) +
                                         upper[i] * (- x[i]^6 + 3 * x[i]^4 - 3 * x[i]^2 + 1) +
                                         upper[i]^2 * (3 * x[i]^5 - 6 * x[i]^3 + 3 * x[i]) +
                                         upper[i]^6 * (3 * x[i]^5 - 16 * x[i]^3 + 10 * x[i]) - (3 * x[i]^5) / 4 +
                                         upper[i]^8 * (- (3 * x[i]^5) / 4 + 9 * x[i]^3 - (15 * x[i]) / 2) +
                                         upper[i]^4 * (- (9 * x[i]^5) / 2 + 14 * x[i]^3 - (15 * x[i]) / 2) +
                                         upper[i]^9 * ((5 * x[i]^4) / 3 - 7 * x[i]^2 + 5 / 3) + (64 * x[i]^4) / 105 + x[i]^3 +
                                         upper[i]^10 * (3 * x[i] - 2 * x[i]^3) + upper[i]^11 * ((15 * x[i]^2) / 11 - 6 / 11) - (256 * x[i]^2) / 385 -
                                         (upper[i]^12 * x[i]) / 2 - x[i] / 2 + upper[i]^13 / 13 + 1024 / 3003)
          } else if (upper[i] >= 1 | upper[i] == Inf) {
            ret[i] = 350 / 429 - (35 * x[i]^2) / 22 + (35*  x[i]^4) / 24 - (35 *  x[i]^6) / 32 + (35 * x[i]^7) / 64 -
                      (35 * x[i]^9) / 768 + (35 * x[i]^11) / 11264 - (175 * x[i]^13) / 1757184
          }
        } else if (x[i] <= 0 & x[i] >= -2) {
          if (upper[i] <= -1) {
            ret[i] = 0
          } else if (upper[i] <= x[i] + 1 & upper[i] >= -1) {
            ret[i] = (35/32)^2 * (- (16 * x[i]^6) / 35 + upper[i]^7 * (x[i]^6 / 7 - (48 * x[i]^4) / 7 + (102 * x[i]^2) / 7 - 20 / 7) +
                                    upper[i]^5 * (- (3 * x[i]^6) / 5 + (54 * x[i]^4) / 5 - (78 * x[i]^2) / 5 + 3) +
                                    upper[i] * (- x[i]^6 + 3 * x[i]^4 - 3 * x[i]^2 + 1) +
                                    upper[i]^2 * (3 * x[i]^5 - 6 * x[i]^3 + 3 * x[i]) +
                                    upper[i]^6 * (3 *x[i]^5 - 16 *x[i]^3 + 10 * x[i]) -(3 * x[i]^5) / 4 +
                                    upper[i]^8 * (- (3 * x[i]^5) / 4 + 9 * x[i]^3 - (15 * x[i]) / 2) +
                                    upper[i]^4 * (- (9 * x[i]^5) / 2 + 14 * x[i]^3 - (15 * x[i]) / 2) +
                                    upper[i]^9 * ((5 * x[i]^4) / 3 - 7 *x[i]^2 + 5 / 3) + (64 * x[i]^4) / 105 + x[i]^3 +
                                    upper[i]^10 * (3 * x[i] - 2 * x[i]^3) + upper[i]^11 * ((15 * x[i]^2) / 11 - 6 / 11) -
                                    (256 * x[i]^2) / 385 - (upper[i]^12 * x[i]) / 2 - x[i] / 2 + upper[i]^13 / 13 + 1024 / 3003)
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
