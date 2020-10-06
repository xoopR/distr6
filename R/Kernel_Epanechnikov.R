#' @title Epanechnikov Kernel
#'
#' @description Mathematical and statistical functions for the Epanechnikov kernel defined by the
#'  pdf, \deqn{f(x) = \frac{3}{4}(1-x^2)}{f(x) = 3/4(1-x^2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The quantile function is omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Epanechnikov
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Epanechnikov <- R6Class("Epanechnikov",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Epanechnikov",
    short_name = "Epan",
    description = "Epanechnikov Kernel",

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
            ret[i] = (9 / 16) * (-abs(x[i])^5 + 20 * abs(x[i])^3 -
                                   40 * abs(x[i])^2 + 32) / 30
          }
        } else{
          if (x[i] >= 0 & x[i] <= 2) {
            if (upper[i] >= 1) {ret[i] = 3*(-x[i]^5 + 20*x[i]^3 - 40*x[i]^2 + 32)/160}
            else if (upper[i] >= (x[i] - 1) & upper[i] <= 1) {
              ret[i] = (3 / 160) * (-x[i]^(5) + 20 * x[i]^(3) + 10 * (x[i]^(2)) * (upper[i]^(3) - 3 * upper[i] - 2) -
                        15 * x[i] * (upper[i]^(2) - 1)^(2) + 6 * upper[i]^(5) - 20 * upper[i]^(3) + 30 * upper[i] + 16)
            } else if (upper[i] <= x[i] -1) {
                ret[i] = 0
            }
          } else if (x[i] >= -2 & x[i] <= 0){
            if(upper[i] >= (x[i] + 1)) {
              ret[i] = 3 * (x[i]^5 - 20 * x[i]^3 - 40 * x[i]^2 + 32) / 160
              } else if (upper[i] >= -1 & upper[i] <= (x[i] + 1)) {
                ret[i] = (3 / 160) * (10 * x[i]^(2) * (upper[i]^(3) - 3 * upper[i] - 2) -
                        15 * x[i] * (upper[i]^(2) - 1)^(2) + 6 * upper[i]^(5) -
                        20 * upper[i]^(3) + 30 * upper[i] + 16)
              } else if (upper[i] <= -1) {
                  ret[i] = 0
                  }
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
    cdfSquared2Norm = function(x = 0, upper = 0) {

      ret <- numeric(length(x))

      for (i in seq_along(x)){

        if(x[i] >= 0 & x[i] <= 2) {
          if(upper[i] <= -1) {
            ret[i] = 0
          } else if(upper[i] >= -1 & upper[i] <= x[i]-11) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] = (x[i]^7 - 42 * x[i]^5 + 105 * x[i]^3 + 84 * x[i]^2 - 280 * x[i] + 132) / 2240 +
              (upper[i] * (280 * x[i]^3 - 840 * x[i] + 560)) / 2240 +
              (upper[i]^2 * (210 * x[i]^3 - 420 * x[i]^2 - 630 * x[i] + 840)) / 2240 +
              (upper[i]^4 * (-35 * x[i]^3 + 420 * x[i] - 140)) / 2240 +
              (upper[i]^5 * (84 * x[i]^2 - 168)) / 2240 + (upper[i]^3 * (-420 * x[i]^2 + 280 * x[i] + 420)) / 2240 -
              (upper[i]^6 * x[i]) / 32 + upper[i]^7 / 112
          } else if (upper[i] >= 1 & upper[i] <= x[i]+1) {
            ret[i] = (x[i]^7 - 42 * x[i]^5 + 560 * upper[i] * x[i]^3 + (168 - 840 * upper[i]^2) * x[i]^2 +
                        (560 * upper[i]^3 - 1680 * upper[i]) * x[i] - 140 * upper[i]^4 + 840 * upper[i]^2 +
                        1120 * upper[i] - 156) / 2240
          } else if (upper[i] >= x[i] + 1) {
            ret[i] = (x[i]^7 - 42 * x[i]^5 + 140 * x[i]^4 - 672 * x[i]^2 - 1120 * x[i] + 2240 * upper[i] - 576) / 2240
          }
        } else if (x[i] >= -2 & x[i] <= 0) {
          if(upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= -1) {
            ret[i] = 0
          } else if(upper[i] >= -1 & upper[i] <= x[i] + 1){
            ret[i] = (upper[i] * (280 * x[i]^3 - 840 * x[i] + 560)) / 2240 +
              (upper[i]^2 * (210 * x[i]^3 - 420 * x[i]^2 - 630 * x[i] + 840)) / 2240 +
              (105 * x[i]^3 + 84 * x[i]^2 - 280 * x[i] + 132) / 2240 +
              (upper[i]^4 * (-35 * x[i]^3 + 420 * x[i] - 140)) / 2240 +
              (upper[i]^5 * (84 * x[i]^2 - 168)) / 2240 +
              (upper[i]^3 * (-420 * x[i]^2 + 280 * x[i] + 420)) / 2240 -
              (upper[i]^6 * x[i]) / 32 + upper[i]^7 / 112
          } else if (upper[i] >= x[i] + 1 & upper[i] <= 1) {
            ret[i] = -x[i]^7 / 2240 + (3 * x[i]^5) / 160 + x[i]^4 / 16 -
              (3 * x[i]^2) / 10 - x[i] / 2 - upper[i]^4 / 16 + (3 * upper[i]^2) / 8 + upper[i] / 2 - 39 / 560
          } else if (upper[i] >= 1) {
            ret[i] = (-x[i]^7 + 42 * x[i]^5 + 140 * x[i]^4 - 672 * x[i]^2 - 1120 * x[i] + 2240 * upper[i] - 576) / 2240
          }
        }  else if (x[i] >= 2) {
          if (upper[i] <=  -1) {
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= 1) {
            ret[i] = 0
          } else if (upper[i] >= 1 & upper[i] <= x[i] - 1){
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i] + 1) {
            ret[i] = -((-x[i] + upper[i] - 3) * (-x[i] + upper[i] + 1)^3) / 16
          } else if (upper[i] >= x[i] + 1) {
            ret[i] = upper[i] - x[i]
          }
        } else if (x[i] <= -2) {
          if (upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i] + 1){
            ret[i] = 0
          } else if (upper[i] >= x[i] + 1 & upper[i] <= -1) {
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= 1) {
            ret[i] = (1/16)*(3 + 8 * upper[i] + 6 * upper[i]^2 - upper[i]^4)
          } else if (upper[i] >= 1) {
            ret[i]= upper[i]
          }
        }
      }
      return(ret)
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
    .isQuantile = 0L,
    .pdf = function(x, log = FALSE) {
      C_EpanechnikovKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_EpanechnikovKernelCdf(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Epan", ClassName = "Epanechnikov",
    Support = "[-1,1]", Packages = "-"
  )
)
