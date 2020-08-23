#' @title Cosine Kernel
#'
#' @description Mathematical and statistical functions for the Cosine kernel defined by the pdf,
#' \deqn{f(x) = (\pi/4)cos(x\pi/2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name Cosine
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Cosine <- R6Class("Cosine",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Cosine",
    short_name = "Cos",
    description = "Cosine Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {

      ret <- numeric(length(x))
      for (i in seq_along(x)) {
        if(abs(x[i] >= 2)) {
          ret[i] = 0
        } else if (x[i] >= 0 & x[i] <= 2) {
          if (upper[i] == Inf | upper[i] >= 1) {
            ret[i] = (pi/32) * (2 * sin((pi * x[i]) / 2) - (x[i] - 2) * pi * cos(pi * x[i] /2))
          } else if (upper[i] >= (x[i] - 1) & upper[i] <= 1) {
            ret[i] = (pi)/(32) * (-sin((pi * x[i] - 2 * pi * upper[i]) / (2)) + sin((pi * x[i]) / (2)) -
                                  (pi * x[i] - pi * upper[i]-pi) * cos((pi * x[i]) / (2)))
          } else if (upper[i] <= x[i] - 1){
            ret[i] = 0
          }
        } else if (x[i] <= 0 & x[i] >= -2) {
          if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = (pi) / (32) * (-2 * sin((pi*x[i]) / (2)) + (x[i]+2) * pi * cos((pi * x[i]) / (2)))
          } else if (upper[i] <= x[i] + 1 & upper[i] >= -1) {
            ret[i] = (pi) / (32) * (-sin((pi * x[i] - 2 * pi * upper[i]) / (2)) + sin((pi * x[i]) / (2)) +
                                    (pi * upper[i] + pi) * cos((pi * x[i]) / (2)))
          } else if (upper[i] <= -1) {
            ret[i] = 0
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
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] =  (1/(8 * pi)) * (2 * pi + 2 * upper[i] * pi - 2 * x[i] * pi - 4 * cos((upper[i] * pi) /2 ) + (1 + upper[i] - x[i]) * pi * cos((x[i] * pi) / 2) -
                    4 * cos((1 / 2) * (-upper[i] + x[i]) * pi) + 3 * sin((x[i] * pi) / 2) + sin((1 / 2) * (-2 * upper[i] + x[i]) * pi))
          } else if (upper[i] >= 1 & upper[i] <= x[i] + 1) {
            ret[i] = (2 * (sin((pi * x[i]) / 2) - cos((pi * (x[i] - upper[i])) / 2))) / pi - sin((pi * x[i]) / 2) / (4 * pi) -
              ((x[i] - 2) * (cos((pi * x[i]) / 2) + 2)) / 8 + upper[i] - 1 / 2
          } else if (upper[i] >= x[i] + 1) {
            ret[i] = (6 * sin((pi * x[i]) / 2) - pi * ((x[i] - 2) * cos((pi * x[i]) / 2) + 6 * x[i] - 8 * upper[i] + 4)) / (8 * pi)
          }
        } else if (x[i] >= -2 & x[i] <= 0) {
          if(upper[i] <= x[i] -1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] -1 & upper[i] <= - 1) {
            ret[i] = 0
          } else if (upper[i] >= - 1 & upper[i] <= x[i] + 1) {
            ret[i] = (-4 * cos((pi * (x[i] - upper[i])) / 2) + sin((pi * (x[i] - 2 * upper[i])) / 2) - 3 * sin((pi * x[i]) / 2) + pi * (upper[i] + 1) *
                     cos((pi * x[i]) / 2) - 4 * cos((pi * upper[i]) / 2) + 2 * pi * upper[i] + 2 * pi) / (8 * pi)
          } else if (upper[i] >= x[i] + 1 & upper[i] <= 1) {
            ret[i] = - (6 * sin((pi * x[i]) / 2) - pi * ((x[i] + 2) * cos((pi * x[i]) / 2) - 2 * x[i] + 4 * upper[i]) +
                       8 * cos((pi * upper[i]) / 2)) / (8 * pi)
          } else if (upper[i] >= 1) {
            ret[i] = (sin((pi * x[i]) / 2) / pi + ((x[i] + 2) * cos((pi * x[i]) / 2)) / 2 + x[i] + 2) / 4 -
              sin((pi * x[i]) / 2) / pi - x[i] / 2 + upper[i]-1
          }
        } else if (x[i] >= 2) {
          if (upper[i] <=  -1) {
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= 1) {
            ret[i] = 0
          } else if (upper[i] >= 1 & upper[i] <= x[i] - 1){
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i] + 1) {
            ret[i] = (-(2 * cos((pi * (x[i] - upper[i])) / 2)) / pi - x[i] + upper[i] + 1) / 2
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
            ret[i] = (-(2 * cos((pi * upper[i]) / 2)) / pi + upper[i] + 1) / 2
          } else if (upper[i] >= 1) {
            ret[i] = upper[i]
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
      return(1 - 8 / (pi^2))
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_CosineKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_CosineKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_CosineKernelQuantile(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Cos", ClassName = "Cosine",
    Support = "[-1,1]", Packages = "-"
  )
)
