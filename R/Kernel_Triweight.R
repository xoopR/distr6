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
    cdfSquared2Norm = function(x = 0, upper = 0) {

      ret <- numeric(length(x))

      for(i in seq_along(x)){

        if (x[i] >= 0 & x[i] <= 2) {
          if(upper[i] <= -1) {
            ret[i]= 0
          } else if (upper[i] >= -1 & upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] =  (1/10543104) * (8 * (1 + upper[i])^9 * (54740 + 3 * upper[i] * (-54396 + upper[i] * (72924 + 55 * upper[i]*(-1011 + upper[i]*(459 + 13*(-9 + upper[i])*upper[i]))))) -
                      5148 * (1 + upper[i])^8 * (-16 + upper[i] * (29 + 5 * (-4 + upper[i]) * upper[i]))^2 * x[i] +
                      840 * (1 + upper[i])^7 * (872 + upper[i] * (-6104 + upper[i] * (14120 + 9 * upper[i] * (-1799 + upper[i] * (1137 + 55 * (-7 + upper[i]) * upper[i])))))*
                      x[i]^2 - 30030 * (-1 + upper[i])^2 * (1 + upper[i])^6 * (-35 + upper[i] * (-52 + upper[i] * (138 + 25 * (-4 + upper[i]) * upper[i])))*x[i]^3 +
                      10920 * (1 + upper[i])^5 * (-68 + upper[i] * (340 + upper[i] * (-228 + 5 * upper[i] * (-85 + upper[i] * (137 + 15 * (-5 + upper[i]) * upper[i])))))*x[i]^4 -
                      18018 * (1 + upper[i])^5 * (35 + upper[i] * (17 + 15 * upper[i] * (-15 + upper[i] * (19 + 2 * (-5 + upper[i]) * upper[i])))) * x[i]^5 +
                      40040 * (-2 + upper[i]) * (1 + upper[i])^5 * (-4 + upper[i] * (18 + 5 * (-3 + upper[i]) * upper[i])) * x[i]^6 -
                      6435 * (1 + upper[i])^5 * (-35 + upper[i] * (47 + 5 * (-5 + upper[i]) * upper[i])) * x[i]^7 - 80080 * x[i]^9 +
                      4368 * x[i]^11 - 210 * x[i]^13 + 5 * x[i]^15)
          } else if (upper[i] >= 1 & upper[i] <= x[i] + 1) {
            ret[i] = 1042 / 1287 - x[i] / 2 - (175 * x[i]^2) / 429 + (35 * x[i]^4) / 264 + (7 * x[i]^5) / 16 - (35 * x[i]^6) /
              72 + (5 * x[i]^7) / 32 - (35 * x[i]^9) / 4608 + (7 * x[i]^11) / 16896 - (35 * x[i]^13) / 1757184 + (5 * x[i]^15) / 10543104 +
              ((1 / 32) * (-(67 / 2) + 16 * upper[i] + (35 * upper[i]^2) / 2 + (35/4) * (- (upper[i] - x[i])^4 + (-1 + x[i])^4) +
                         (7 / 2) * ((upper[i] - x[i])^6 - (-1 + x[i])^6) + (5 / 8) * (-(upper[i] - x[i])^8 + (-1 + x[i])^8) + 35 * x[i] - 35 * upper[i] * x[i]))
          } else if (upper[i] >= x[i] + 1) {
            ret[i] = 1042 / 1287 - x[i] / 2 - (175 * x[i]^2) / 429 + (35 * x[i]^4) / 264 + (7 * x[i]^5) / 16 - (35 * x[i]^6) / 72 + (5 * x[i]^7) / 32 -
              (35 * x[i]^9) / 4608 + (7 * x[i]^11) / 16896 - (35 * x[i]^13) / 1757184 + (5 * x[i]^15) / 10543104 +
              (x[i]- (7 * x[i]^5) / 16 + (7 * x[i]^6) / 16 - (5 * x[i]^7) / 32 + (5 * x[i]^8) / 256) + (upper[i] - x[i] -1)
          }
        } else if (x[i] >= -2 & x[i] <= 0) {
          if(upper[i] <= x[i] -1) {
            ret[i]= 0
          } else if (upper[i] >= x[i] -1 & upper[i] <= - 1) {
            ret[i] = 0
          } else if (upper[i] >= - 1 & upper[i] <= x[i] + 1) {
            ret[i] =  (1/10543104) * ((1 + upper[i])^5 * (8 * (1 + upper[i])^4 * (54740 + 3 * upper[i] * (-54396 +
                      upper[i] * (72924 + 55 * upper[i] * (-1011 + upper[i] * (459 + 13 * (-9 + upper[i]) * upper[i]))))) -
                      5148 * (1 + upper[i])^3 * (-16 + upper[i] * (29 + 5 * (-4 + upper[i]) * upper[i]))^2 * x[i] +
                      840 * (1 + upper[i])^2 * (872 + upper[i] * (-6104 + upper[i] * (14120 + 9 * upper[i] * (-1799 + upper[i] * (1137 + 55 * (-7 + upper[i]) * upper[i]))))) *
                      x[i]^2 - 30030 * (-1 + upper[i])^2 * (1 + upper[i]) * (-35 + upper[i] * (-52 + upper[i] * (138 + 25 * (-4 + upper[i]) * upper[i]))) * x[i]^3 +
                      10920 * (-68 + upper[i] * (340 + upper[i] * (-228 + 5 * upper[i] * (-85 + upper[i] * (137 + 15 * (-5 + upper[i]) * upper[i]))))) *
                      x[i]^4 - 18018 * (35 + upper[i] * (17 + 15 * upper[i] * (-15 + upper[i] * (19 + 2 * (-5 + upper[i]) * upper[i]))))*
                      x[i]^5 + 40040 * (-2 + upper[i]) * (-4 + upper[i] * (18 + 5 * (-3 + upper[i]) * upper[i])) * x[i]^6 -
                      6435 * (-35 + upper[i] * (47 + 5 * (-5 + upper[i]) * upper[i])) * x[i]^7))
          } else if (upper[i] >= x[i] + 1 & upper[i] <= 1) {
            ret[i] = 1042 / 1287 + x[i] / 2 - (175 * x[i]^2) / 429 + (35 * x[i]^4) / 264 - (7 * x[i]^5) / 16 - (35 * x[i]^6) /  72 -
              (5 * x[i]^7) / 32 + (35 * x[i]^9) / 4608 - (7 * x[i]^11) / 16896 + (35 * x[i]^13) / 1757184 - (5 * x[i]^15) / 10543104 +
              (-(221/256) + upper[i] / 2 + (35 * upper[i]^2) / 64 - (35 * upper[i]^4) / 128 + (7 * upper[i]^6) / 64 - (5 * upper[i]^8) /
                 256 - x[i] + (7 * x[i]^5) / 16 + (7 * x[i]^6) / 16 + (5 * x[i]^7) / 32 + (5 * x[i]^8) / 256)
          } else if (upper[i] >= 1) {
            ret[i] = 1042 / 1287 + x[i] / 2 - (175 * x[i]^2) / 429 + (35 * x[i]^4) / 264 - (7 * x[i]^5) / 16 - (35 * x[i]^6) / 72 - (5 * x[i]^7) / 32 +
              (35 * x[i]^9) / 4608 - (7 * x[i]^11) / 16896 + (35 * x[i]^13) / 1757184 - (5 * x[i]^15) / 10543104 +
              (-x[i] + (7 * x[i]^5) / 16 + (7 * x[i]^6) / 16 + (5 * x[i]^7) / 32 + (5 * x[i]^8) / 256) + (upper[i] - 1)
          }
        } else if (x[i] >= 2) {
          if (upper[i] <=  -1) {
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= 1) {
            ret[i] = 0
          } else if (upper[i] >= 1 & upper[i] <= x[i] - 1){
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i] + 1) {
            ret[i] = (-(1 / 256)) * (1 + upper[i] - x[i])^5 * (- 35 + 5 * upper[i]^3 - 47 * x[i] - 25 * x[i]^2 - 5 * x[i]^3 -
                      5 * upper[i]^2 * (5 + 3 * x[i]) + upper[i] * (47 + 50 * x[i] + 15 * x[i]^2))
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
            ret[i] = 35 / 256 + upper[i] / 2 + (35 * upper[i]^2) / 64 - (35 * upper[i]^4) / 128 + (7 * upper[i]^6) / 64 -
              (5 * upper[i]^8) / 256
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
      return(1 / 9)
    }
  ),

  private = list(
    .isQuantile = 0L,
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
