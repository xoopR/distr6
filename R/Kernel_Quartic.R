#' @title Quartic Kernel
#'
#' @description Mathematical and statistical functions for the Quartic kernel defined by the pdf,
#' \deqn{f(x) = 15/16(1 - x^2)^2}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details Quantile is omitted as no closed form analytic expression could be found, decorate with
#' FunctionImputation for numeric results.
#'
#' @name Quartic
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Quartic <- R6Class("Quartic",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Quartic",
    short_name = "Quart",
    description = "Quartic Kernel",

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
          if (upper[i] <= 0) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] = (15/16)^2 * (1/630) * (256 + 630 * upper[i] - 840 * (upper[i])^3 + 756 * (upper[i])^5 - 360 * (upper[i])^7 +
                     70 * (upper[i])^9 - 315 * x[i] + 1260 * (upper[i])^2 * x[i] - 1890 * (upper[i])^4 * x[i] +
                     1260 * (upper[i])^6 * x[i] - 315 * (upper[i])^8 * (x[i]) - 384 * (x[i])^2 - 1260 * (upper[i]) * (x[i])^2 +
                     2100 * (upper[i])^3 * (x[i])^2 - 1764*(upper[i])^5 * (x[i])^2 + 540 * (upper[i])^7 * (x[i])^2 + 420 * (x[i])^3 -
                     1260 * (upper[i])^2 *(x[i])^3 + 1260*(upper[i])^4 * (x[i])^3 - 420 * (upper[i])^6 * (x[i])^3 + 336 * (x[i])^4 +
                     630 * (upper[i]) * (x[i])^4 - 420*(upper[i])^3 * (x[i])^4 + 126 * (upper[i])^5 * (x[i])^4 - 336 * (x[i])^5 +
                     24 * (x[i])^7 - (x[i])^9)
          } else if (upper[i] >= 1 | upper[i] == Inf){
            ret[i] = (15/16)^2 * (1/630) * (-x[i]^(9) + 24 * x[i]^(7) - 336 * x[i]^(5) + 672 * x[i]^(4) - 768 * x[i]^(2) + 512)
          }
        } else if (x[i] <= 0) {
          if (upper[i] <= -1) {
            ret[i] = 0
          } else if (upper[i] <= x[i] + 1 & upper[i] >= -1) {
            ret[i] = (15/16)^(2) * (1/630) * ((126 * (upper[i])^(5) - 420 * (upper[i])^(3) + 630 * (upper[i]) + 336) * (x[i])^(4) +
                     (-420 * (upper[i])^(6) + 1260 * (upper[i])^(4) - 1260 * (upper[i])^(2) + 420) * (x[i])^(3) +
                     (540 * (upper[i])^(7) - 1764 * (upper[i])^(5) + 2100 * (upper[i])^(3) - 1260 * (upper[i]) - 384) * (x[i])^(2) +
                     (-315 * (upper[i])^(8) + 1260 * (upper[i])^(6) - 1890 * (upper[i])^(4) + 1260 * (upper[i])^(2) - 315) * x[i] +
                      70 * (upper[i])^(9) - 360 * (upper[i])^(7) + 756 * (upper[i])^(5) - 840 * (upper[i])^(3) + 630 * (upper[i]) + 256)
          } else if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = (15/16)^2 * (1/630) *(x[i]^(9)-24*x[i]^(7)+336*x[i]^(5)+672*x[i]^(4)-768*x[i]^(2)+512)
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
      return(1 / 7)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_QuarticKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_QuarticKernelCdf(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Quart", ClassName = "Quartic",
    Support = "[-1,1]", Packages = "-"
  )
)
