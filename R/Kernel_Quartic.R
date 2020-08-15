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
        if (abs(x[i]) >= 2) {
          ret[i] = 0
        } else if (x[i] >= 0) {
          if (upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] = (15/16)^(2) * (-x[i]^9 / 630 + (4 * x[i]^7) / 105 -
              (8 * x[i]^5) / 15 + upper[i] * (x[i]^4 - 2 * x[i]^2 + 1) +
              (8 * x[i]^4) / 15 + upper[i]^5 * (x[i]^4 / 5 - (14 * x[i]^2) / 5 + 6 / 5) +
              upper[i]^3 * (- (2 * x[i]^4) / 3 + (10 * x[i]^2) / 3 - 4 / 3) +
              upper[i]^4 * (2 * x[i]^3 - 3 * x[i]) + (2 * x[i]^3) / 3 +
              upper[i]^6 * (2 * x[i] - (2 * x[i]^3) / 3) +
              upper[i]^2 * (2 * x[i] - 2 * x[i]^3) - (64 * x[i]^2) / 105 +
              upper[i]^7 * ((6 * x[i]^2) / 7 - 4 / 7) - (upper[i]^8 * x[i]) / 2 - x[i] / 2 +
              upper[i]^9 / 9 + 128 / 315)
          } else if (upper[i] >= 1 | upper[i] == Inf){
            ret[i] = (15 / 16)^2 * (1 / 630) * (-x[i]^(9) + 24 * x[i]^(7) - 336 * x[i]^(5) + 672 * x[i]^(4) - 768 * x[i]^(2) + 512)
          }
        } else if (x[i] <= 0) {
          if (upper[i] <= -1) {
            ret[i] = 0
          } else if (upper[i] <= x[i] + 1 & upper[i] >= -1) {
            ret[i] = (15 / 16)^(2) * (upper[i] * (x[i]^4 - 2 * x[i]^2 + 1) + (8 * x[i]^4) / 15 +
                                        upper[i]^5 * (x[i]^4 / 5 - (14 * x[i]^2) / 5 + 6 / 5) +
                                        upper[i]^3 * (- (2 * x[i]^4) / 3 + (10 * x[i]^2) / 3 - 4 / 3) +
                                        upper[i]^4 * (2 * x[i]^3 - 3 * x[i]) + (2 * x[i]^3) / 3 +
                                        upper[i]^6 * (2 * x[i] - (2 * x[i]^3) / 3) +
                                        upper[i]^2 * (2 * x[i] - 2 * x[i]^3) - (64 * x[i]^2) / 105 +
                                        upper[i]^7 * ((6 * x[i]^2) / 7 - 4 / 7) - (upper[i]^8 * x[i]) / 2 - x[i] /2 + upper[i]^9 / 9 + 128 / 315)
          } else if (upper[i] == Inf | upper[i] >= x[i] + 1) {
            ret[i] = (15 / 16)^2 * (1 / 630) * (x[i]^(9) - 24 * x[i]^(7) + 336 * x[i]^(5) + 672 * x[i]^(4) - 768 * x[i]^(2) + 512)
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

      #when c \in [ 0, 2]
      #i. when a \in [c-1, 1]
      f1 <- function(x, upper){
        ret = (1/236544)*((1 + upper - x)^4*(11360 + 756*upper^7 + 15872*x + 8528*x^2 + 1645*x^3 - 380*x^4 -
                                               80*x^5 + 12*x^6 + 3*x^7 - 378*upper^6*(8 + 3*x) +
                                               56*upper^5*(25 + 27*x + 3*x^2) +
                                               35*upper^4*(272 + 196*x + 24*x^2 + 3*x^3) +
                                               20*upper^3*(-503 - 518*x - 26*x^2 + 21*x^3 + 3*x^4) +
                                               2*upper^2*(-6032 - 6855*x - 2460*x^2 - 235*x^3 + 90*x^4 + 15*x^5) +
                                               4*upper*(3424 + 4184*x + 1200*x^2 - 445*x^3 - 65*x^4 + 15*x^5 +
                                                          3*x^6)))
        return(ret)
      }

      #ii. when upper \in [1, x+1]
      f2 <- function(x, upper){
        ret = (1/32)*(-31 + 16*upper + 15*upper^2 - 5*(upper - x)^4 + (upper - x)^6 +
                        5*(-1 + x)^4 - (-1 + x)^6 + 30*x - 30*upper*x)
        return(ret)
      }


      #2. c \ge 2
      f3 <- function(x, upper){
        ret = (1/32)*(1 + upper - x)^4*(5 + upper^2 + 4*x + x^2 - 2*upper*(2 + x))
        return(ret)
      }

      #3. x \in [-2, 0]
      #i. upper \in [-1, x+1]

      f4 <- function(x, upper){
        ret = (1/236544)*((1 + upper)^4*(4*(1 + upper)^3*(2840 + 7*upper*(-728 + upper*(536 + 27*(-7 + upper)*upper))) -
                                           462*(-8 + upper + 6*upper^2 - 3*upper^3)^2*x +
                                           1320*(1 + upper)*(10 + upper*(-50 + upper*(66 + 7*(-5 + upper)*upper)))*x^2 -
                                           1155*(-1 + upper)*(15 + upper*(19 + 9*(-3 + upper)*upper))*x^3 +
                                           1980*(-4 + upper*(16 + 3*(-4 + upper)*upper))*x^4 - 1386*(5 + (-4 + upper)*upper)*x^5))
        return(ret)
      }


      #ii. upper \in [x+1, 1]

      f5 <- function(x, upper){
        ret = -(27/32) + upper/2 + (15*upper^2)/32 - (5*upper^4)/32 +
          upper^6/32 - x - (5*x^4)/16 - (3*x^5)/16 - x^6/32
        return(ret)
      }

      #4. x \le -2
      f6 <- function(x, upper){
        ret = 5/32 + upper/2 + (15*upper^2)/32 - (5*upper^4)/32 + upper^6/32
        return(ret)
      }

      ret <- numeric(length(x))

      for(i in seq_along(x)){

        if (x[i] >= 0 & x[i] <= 2) {
          if(upper[i] <= -1) {
            ret[i]= 0
          } else if (upper[i] >= -1 & upper[i] <= x[i] - 1) {
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
            ret[i] =  f1(x = x[i], upper = upper[i])
          } else if (upper[i] >= 1 & upper[i] <= x[i] + 1) {
            ret[i] = f2(x = x[i], upper = upper[i]) + f1(x = x[i], upper = 1)
          } else if (upper[i] >= x[i] + 1) {
            ret[i] = f2(x = x[i], upper = x[i] + 1) + f1(x = x[i], upper = 1) + (upper[i] - x[i] - 1)
          }
        } else if (x[i] >= -2 & x[i] <= 0) {
          if(upper[i] <= x[i] -1) {
            ret[i]= 0
          } else if (upper[i] >= x[i] -1 & upper[i] <= - 1) {
            ret[i] = 0
          } else if (upper[i] >= - 1 & upper[i] <= x[i] + 1) {
            ret[i] =  f4(x = x[i], upper = upper[i])
          } else if (upper[i] >= x[i] + 1 & upper[i] <= 1) {
            ret[i] = f5(x = x[i], upper= upper[i]) +  f6(x = x[i], upper = x[i] + 1)
          } else if (upper[i] >= 1) {
            ret[i] = f6(x = x[i], upper= 1) +  f5(x = x[i], upper = x[i] + 1) + (upper[i] - 1)
          }
        } else if (x[i] >= 2) {
          if (upper[i] <=  -1) {
            ret[i] = 0
          } else if (upper[i] >= -1 & upper[i] <= 1) {
            ret[i] = 0
          } else if (upper[i] >= 1 & upper[i] <= x[i] - 1){
            ret[i] = 0
          } else if (upper[i] >= x[i] - 1 & upper[i] <= x[i] + 1) {
            ret[i] = f3(x = x[i], upper = upper[i])
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
            ret[i] = f6(x = x[i], upper = upper[i])
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
