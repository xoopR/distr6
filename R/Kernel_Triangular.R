#' @title Triangular Kernel
#'
#' @description Mathematical and statistical functions for the Triangular kernel defined by the pdf,
#' \deqn{f(x) = 1 - |x|}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name TriangularKernel
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#'
#' @export
TriangularKernel <- R6Class("TriangularKernel",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "TriangularKernel",
    short_name = "Tri",
    description = "Triangular Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {
      ret = numeric(length(x))
      for (i in seq_along(x)) {
        if (x[i] >= 0 & x[i] <= 1) {
          if (upper[i] >= (x[i] - 1) & upper[i] <= 0) {
            ret[i] = (x[i]^3 + (-3 * upper[i]^2 - 6 * upper[i] - 3) * x[i] + 2 * upper[i]^3 + 6 * upper[i]^2 + 6 * upper[i] + 2)/6
          } else if (upper[i] >= 0 & upper[i] <= x[i]) {
            ret[i] = (-2 * upper[i]^3 + 3 * upper[i]^2 * x[i] - 6 * upper[i] * x[i] + 6 * upper[i] + x[i]^3 - 3 * x[i] + 2) / 6
          } else if (upper[i] >= x[i] & upper[i] <= 1) {
            ret[i] = (2 * upper[i]^3 - 3 * upper[i]^2 * x[i] - 6 * upper[i]^2 + 6 * upper[i] * x[i] + 6 * upper[i] +
                        3 * x[i]^3 - 6 * x[i]^2 - 3 * x[i] + 2) / 6
          } else if (upper[i] == Inf | upper[i] > 1) {
            ret[i] = (3 * x[i]^3 - 6 * x[i]^2 + 4) / 6
          } else {ret[i] = 0}
        } else if (x[i] >=  1 & x[i] <= 2) {
          if (upper[i] == Inf | upper[i] >= 1) {
            ret[i] = (-x[i]^3 + 6 * x[i]^2 - 12 * x[i] + 8) / 6
          } else if (upper[i] >= (x[i] - 1) & upper[i] <= 1) {
            ret[i] = (- x[i]^3 + 6 * x[i]^2 - (- 3 * upper[i]^2 + 6 * upper[i] + 9) * x[i] - 2 * upper[i]^3 + 6 * upper[i] + 4) / 6
          } else {ret[i] = 0}
        } else if (x[i] >= -1 & x[i] <= 0) {
          if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = (-x[i]^3 + 6 * x[i]^2 + 4) / 6
          } else if (upper[i] >= -1 & upper[i] <= x[i]) {
            ret[i] = (- (3 * upper[i] ^2 + 6 * upper[i]  + 3) * x[i] + 2 * upper[i]^3 + 6 *upper[i] ^2 + 6 * upper[i] + 2) / 6
          } else if (upper[i] >= x[i] & upper[i] <= 0) {
            ret[i] = -(x[i] * (2 * x[i]^2 + 6 * x[i] - 3 * upper[i] * (upper[i] + 2) + 3) +
                         2 * (upper[i]^3 - 3*upper[i] - 1))/6
          } else if (upper[i] >=0 & upper[i] <= x[i] + 1) {
            ret[i] = (2 * upper[i]^3 - 3 * upper[i]^2 * x[i] - 6 * upper[i]^2 + 6 * upper[i] * x[i] + 6 * upper[i] - 2 * x[i]^3 -
                        6 * x[i]^2 - 3 * x[i] + 2) / 6
          } else {ret[i] = 0}
        } else if (x[i] >= -2 & x[i] <= -1 ) {
          if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = (x[i]^3 + 6 * x[i]^2 + 12 * x[i] + 8) / 6
          } else if (upper[i] >= -1 & upper[i] <= x[i] + 1) {
            ret[i] = ((3 * upper[i]^2 + 6 * upper[i]) * x[i] - 2 * upper[i]^3 + 6 * upper[i] + 3 * x[i] + 4) / 6
          }
        } else {ret[i] = 0}
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

          if(x[i] >= 0 & x[i] <= 1) {
            if (upper[i] <= -1) {
              ret[i] = 0
            } else if (upper[i] >= -1 & upper[i] <= x[i] -1) {
              ret[i] = 0
            } else if (upper[i] >= x[i] - 1 & upper[i] <= 0) {
              ret[i] = (-x[i]^5 + 10 * x[i]^2 - 15 * x[i] + 6) / 120 + (upper[i] * (30 * x[i]^2 - 60 * x[i] + 30)) / 120 +
                (upper[i]^2 * (30 * x[i]^2 - 90 * x[i] + 60)) / 120 +
                (upper[i]^3 * (10 * x[i]^2 - 60 * x[i] + 60)) / 120 + (upper[i]^4 * (30 - 15 * x[i])) / 120 +
                upper[i]^5 / 20
            } else if (upper[i] >= 0 & upper[i] <= x[i]) {
              ret[i] = -x[i]^5 / 120 + (upper[i] * (30 * x[i]^2 - 60 * x[i] + 30)) / 120 +
                (upper[i]^2 * (30 * x[i]^2 - 90 * x[i] + 60)) / 120 + x[i]^2 / 12 +
                (upper[i]^3 * (-10 * x[i]^2 - 20 * x[i] + 40)) / 120 + (upper[i]^4 * x[i]) / 8 - x[i] / 8 - upper[i]^5 / 20 + 1 / 20
            } else if (upper[i] >= x[i] & upper[i] <= 1) {
              ret[i] = -x[i]^5 / 60 + (-x[i]^5 + 40 * x[i]^3 - 30 * x[i]) / 120 +
                x[i]^4 / 12 - x[i]^3 / 6 + (upper[i]^3 * (10 * x[i]^2 + 60 * x[i] + 20)) / 120 + x[i]^2 / 12 +
                (upper[i]^2 * (-30 * x[i]^2-30 * x[i]+60)) / 120 + (upper[i] * (-30 * x[i]^2 - 60 * x[i] + 30)) / 120 +
                x[i] / 8 + (upper[i]^4 * (-15 * x[i] - 30)) / 120 + upper[i]^5 / 20 + 1 / 20
            } else if (upper[i] >= x[i]  & upper[i] <= x[i] + 1){
              ret[i] = (-3 * x[i]^5 + 10 * x[i]^4 + 20 * x[i]^3 + 20 * x[i]^2 - 8) / 120 + (upper[i] * (-60 * x[i]^2 - 120 * x[i] + 60)) / 120 +
                (upper[i]^2 * (60 * x[i] + 60)) / 120 - upper[i]^3 / 6
            } else if (upper[i] >= x[i] + 1) {
              ret[i] = (1 / 120) * (-28 + 120 * upper[i]- 60 * x[i] - 40 * x[i]^2 + 10 * x[i]^4 - 3 * x[i]^5)
            }
          }  else if(x[i] >= 1 & x[i] <= 2) {
            if (upper[i] <= -1) {
              ret[i] = 0
            } else if (upper[i] >= -1 & upper[i] <= x[i] -1) {
              ret[i] = 0
            } else if (upper[i] >= x[i] - 1 & upper[i] <= 0) {
              ret[i] = -(upper[i]^5/20) + (upper[i]^4*x[i])/8 + (1/120)*upper[i]^3*(40 - 20*x[i] - 10*x[i]^2) +
                (1/120)*upper[i]^2*(60 - 90*x[i] + 30*x[i]^2) + (1/120)*upper[i]*(30 - 60*x[i] + 30*x[i]^2) +
                (1/120)*(4 - 5*x[i] - 10*x[i]^2 + 20*x[i]^3 - 10*x[i]^4 + x[i]^5)
            } else if (upper[i] >= 0 & upper[i] <= x[i]) {
              ret[i] = (1/120)* (20*upper[i]^3 - 60 * upper[i]^2* (x[i] - 1) + 60 * upper[i] * (x[i] - 1)^2 + x[i]^5 - 10 * x[i]^4 + 20 * x[i]^3 - 20 * x[i]^2 + 20 * x[i] - 12)
            } else if (upper[i] >= x[i] & upper[i] <= x[i] + 1) {
              ret[i] = (x[i]^5-10*x[i]^4+60*x[i]^3+(-60*upper[i]-20)*x[i]^2+(60*upper[i]^2-120*upper[i]+20)*x[i]-20*upper[i]^3+60*upper[i]^2+60*upper[i]-12)/120
            } else if (upper[i] >= x[i] + 1){
              ret[i] = (x[i]^5-10*x[i]^4+40*x[i]^3-80*x[i]^2-40*x[i]+120*upper[i]-32)/120
            }
          } else if (x[i] >= -1 & x[i] <= 0) {
            if (upper[i] <= x[i] - 1) {
              ret[i] = 0
            } else if (upper[i] >= x[i] - 1 & upper[i] <= -1) {
              ret[i] = 0
            } else if (upper[i] >= -1 & upper[i] <= x[i]) {
              ret[i] = upper[i]^5/20 + (1/120)*upper[i]^4*(30 - 15*x[i]) + (1/120)*upper[i]^3*(60 - 60*x[i] + 10*x[i]^2) +
                (1/120)*(6 - 15*x[i] + 10*x[i]^2) + (1/120)*upper[i]^2*(60 - 90*x[i] + 30*x[i]^2) +
                (1/120)*upper[i]*(30 - 60*x[i] + 30*x[i]^2)
            } else if (upper[i] >= x[i] & upper[i] <= 0) {
              ret[i] = (x[i]^5+10*x[i]^4+20*x[i]^3-30*x[i])/120+x[i]^5/120+x[i]^2/12+(upper[i]^3*(-10*x[i]^2+20*x[i]+40))/120+
                (upper[i]^2*(-30*x[i]^2-30*x[i]+60))/120+(upper[i]*(-30*x[i]^2-60*x[i]+30))/120+(upper[i]^4*x[i])/8+x[i]/8-upper[i]^5/20+1/20
            } else if (upper[i] >= 0 & upper[i] <= x[i] + 1) {
              ret[i] = x[i]^5/60+x[i]^4/12+x[i]^3/6+(upper[i]*(-30*upper[i]*(x[i]^2+x[i]-2)+10*upper[i]^2*(x[i]*(x[i]+6)+2)-30*x[i]*(x[i]+2)-15*upper[i]^3*(x[i]+2)+
                                                                 6*upper[i]^4+30))/120+x[i]^2/12-x[i]/8+1/20
            } else if (upper[i] >= x[i] + 1  & upper[i] <= 1){
              ret[i] = (3*x[i]^5+10*x[i]^4-40*x[i]^2-60*x[i]-20*upper[i]*(upper[i]^2-3*upper[i]-3)-8)/120
            } else if (upper[i] >= 1) {
              ret[i] = (1/120)*(-28 + 120*upper[i]- 60*x[i] - 40*x[i]^2 + 10*x[i]^4 + 3*x[i]^5)
            }
          } else if (x[i] >= -2 & x[i] <= -1) {
            if (upper[i] <= x[i] - 1) {
              ret[i] = 0
            } else if (upper[i] >= x[i] -1 & upper[i] <= -1) {
              ret[i] = 0
            } else if (upper[i] >=-1 & upper[i] <= x[i] + 1) {
              ret[i] =(upper[i]^3*(-10*x[i]^2+20*x[i]+40))/120+(-10*x[i]^2-25*x[i]+4)/120+(upper[i]^2*(-30*x[i]^2-30*x[i]+60))/120+(upper[i]*(-30*x[i]^2-60*x[i]+30))/120+(upper[i]^4*x[i])/8-upper[i]^5/20
            } else if (upper[i] >= x[i] + 1 & upper[i] <= 0) {
              ret[i] = (-x[i]^5-10*x[i]^4-40*x[i]^3-80*x[i]^2-80*x[i]+20*upper[i]^3+60*upper[i]^2+60*upper[i]-12)/120
            } else if (upper[i] >= 0 & upper[i] <= 1) {
              ret[i] = (-x[i]^5-10*x[i]^4-40*x[i]^3-80*x[i]^2-80*x[i]-20*upper[i]*(upper[i]^2-3*upper[i]-3)-12)/120
            } else if (upper[i] >= 1) {
              ret[i] = (-x[i]^5-10*x[i]^4-40*x[i]^3-80*x[i]^2-80*x[i]+120*upper[i]-32)/120
            }
          } else if (x[i] >= 2) {
            if (upper[i] <= x[i] - 1) {
              ret[i] = 0
            } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i]) {
              ret[i] = (1/6)*(1 + upper[i] - x[i])^3
            } else if (upper[i] >= x[i] & upper[i] <= x[i] + 1) {
              ret[i] = (x[i]^3+(3-3*upper[i])*x[i]^2+(3*upper[i]^2-6*upper[i]-3)*x[i]-upper[i]^3+3*upper[i]^2+3*upper[i])/6 + 1/6
            } else if (upper[i] >= x[i] + 1) {
              ret[i] = upper[i]-x[i]
            }
          } else if (x[i] <= -2) {
            if (upper[i] <= -1) {
              ret[i] = 0
            } else if (upper[i] >= -1 & upper[i] <= 0) {
              ret[i] = (upper[i]^3+3*upper[i]^2+3*upper[i]+1)/6
            } else if (upper[i] >= 0 & upper[i] <= 1) {
              ret[i] = (-upper[i]^3+3*upper[i]^2+3*upper[i]+1)/6
            } else if (upper[i] >= 1){
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
      return(1 / 6)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TriangularKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_TriangularKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_TriangularKernelQuantile(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Tri", ClassName = "TriangularKernel",
    Support = "[-1,1]", Packages = "-"
  )
)
