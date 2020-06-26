#' @title Tricube Kernel
#'
#' @description Mathematical and statistical functions for the Tricube kernel defined by the pdf,
#' \deqn{f(x) = 70/81(1 - |x|^3)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Tricube
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Tricube <- R6Class("Tricube",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Tricube",
    short_name = "Tric",
    description = "Tricube Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {

      ret <- numeric(length(x))
      for (i in seq_along(x)) {
        if (upper[i] == Inf) {
          if (abs(x[i]) >= 2) {
            ret[i] = 0
          } else if (abs(x[i]) <= 1) {
            ret[i] = (70 / 81)^2 * (6561 / 6916 - (19683 * x[i]^2) / 13090 + (9 * x[i]^4) / 5 -
                                      (729 * x[i]^6) / 182 +  (747 * abs(x[i])^7) / 140 -
                                      (729 * x[i]^8) /
                                      220 + (81 * abs(x[i])^9) / 70 - (31 * x[i]^10) / 140 +
                                      (111 * abs(x[i])^13) / 20020 - (3 * x[i]^16) / 40040 +
                                      abs(x[i])^19 / 461890)
          } else {
            ret[i] = (70 / 81)^2 * (2592 / 1729 - (16 * abs(x[i])) / 5 + (5832 * abs(x[i])^2) /
                                      935 -
                                      (972 * abs(x[i])^3) / 91 + (66 * abs(x[i])^4) / 5 -
                                      (729 * abs(x[i])^5) / 55 + (9963 * abs(x[i])^6) / 910 -
                                      (969 * abs(x[i])^7) / 140 + (729 * abs(x[i])^8) / 220 -
                                      (81 * abs(x[i])^9) / 70 + (31 * abs(x[i])^10) / 140 -
                                      (57 * abs(x[i])^13) / 20020 + (3 * abs(x[i])^16) /
                                      40040 - abs(x[i])^19 / 923780)
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
      return(35 / 243)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TricubeKernelPdf(x, log)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Tric", ClassName = "Tricube",
    Support = "[-1,1]", Packages = "-"
  )
)
