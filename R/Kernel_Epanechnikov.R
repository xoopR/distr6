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
      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]
        if (ui == Inf) {
          if (abs(xi) > 2) {
            ret[i] <- 0
          } else {
            ret[i] <- (9 / 16) * (-abs(xi)^5 + 20 * abs(xi)^3 -
                                   40 * abs(xi)^2 + 32) / 30
          }
        } else{
          if (xi >= 0 & xi <= 2) {
            if (ui >= 1) {
              ret[i] <- 3 * (-xi^5 + 20 * xi^3 - 40 * xi^2 + 32) / 160
              } else if (ui >= (xi - 1) & ui <= 1) {
              ret[i] <- (3 / 160) * (- xi^5 + 20 * xi^3 +
                                       10 * (xi^2) * (ui^3 - 3 * ui - 2) -
                        15 * xi * (ui^2 - 1)^2 +
                          6 * ui^5 - 20 * ui^3 + 30 * ui + 16)
            } else if (ui <= xi - 1) {
                ret[i] <- 0
            }
          } else if (xi >= -2 & xi <= 0) {
            if (ui >= (xi + 1)) {
              ret[i] <- 3 * (xi^5 - 20 * xi^3 - 40 * xi^2 + 32) / 160
              } else if (ui >= -1 & ui <= (xi + 1)) {
                ret[i] <- (3 / 160) * (10 * xi^2 * (ui^3 - 3 * ui - 2) -
                        15 * xi * (ui^2 - 1)^2 + 6 * ui^5 -
                        20 * ui^3 + 30 * ui + 16)
              } else if (ui <= -1) {
                  ret[i] <- 0
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

      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]

        if (xi >= 0 & xi <= 2) {
          if (ui <= -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= xi - 11) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= 1) {
            ret[i] <- (xi^7 - 42 * xi^5 + 105 * xi^3 + 84 * xi^2 -
                         280 * xi + 132) / 2240 +
              (ui * (280 * xi^3 - 840 * xi + 560)) / 2240 +
              (ui^2 * (210 * xi^3 - 420 * xi^2 - 630 * xi + 840)) / 2240 +
              (ui^4 * (-35 * xi^3 + 420 * xi - 140)) / 2240 +
              (ui^5 * (84 * xi^2 - 168)) / 2240 +
              (ui^3 * (-420 * xi^2 + 280 * xi + 420)) / 2240 -
              (ui^6 * xi) / 32 + ui^7 / 112
          } else if (ui >= 1 & ui <= xi + 1) {
            ret[i] <- (xi^7 - 42 * xi^5 + 560 * ui * xi^3 +
                         (168 - 840 * ui^2) * xi^2 +
                        (560 * ui^3 - 1680 * ui) * xi -
                         140 * ui^4 + 840 * ui^2 +
                        1120 * ui - 156) / 2240
          } else if (ui >= xi + 1) {
            ret[i] <- (xi^7 - 42 * xi^5 + 140 * xi^4 -
                         672 * xi^2 - 1120 * xi + 2240 * ui - 576) / 2240
          }
        } else if (xi >= -2 & xi <= 0) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= xi + 1) {
            ret[i] <- (ui * (280 * xi^3 - 840 * xi + 560)) / 2240 +
              (ui^2 * (210 * xi^3 - 420 * xi^2 - 630 * xi + 840)) / 2240 +
              (105 * xi^3 + 84 * xi^2 - 280 * xi + 132) / 2240 +
              (ui^4 * (-35 * xi^3 + 420 * xi - 140)) / 2240 +
              (ui^5 * (84 * xi^2 - 168)) / 2240 +
              (ui^3 * (-420 * xi^2 + 280 * xi + 420)) / 2240 -
              (ui^6 * xi) / 32 + ui^7 / 112
          } else if (ui >= xi + 1 & ui <= 1) {
            ret[i] <- - xi^7 / 2240 + (3 * xi^5) / 160 + xi^4 / 16 -
                      (3 * xi^2) / 10 - xi / 2 - ui^4 / 16 +
                      (3 * ui^2) / 8 + ui / 2 - 39 / 560
          } else if (ui >= 1) {
            ret[i] <- (- xi^7 + 42 * xi^5 + 140 * xi^4 -
                         672 * xi^2 - 1120 * xi + 2240 * ui - 576) / 2240
          }
        }  else if (xi >= 2) {
          if (ui <=  -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- 0
          } else if (ui >= 1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- (- ((- xi + ui - 3) * (-xi + ui + 1)^3)) / 16
          } else if (ui >= xi + 1) {
            ret[i] <- ui - xi
          }
        } else if (xi <= -2) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- 0
          } else if (ui >= xi + 1 & ui <= -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- (1 / 16) * (3 + 8 * ui + 6 * ui^2 - ui^4)
          } else if (ui >= 1) {
            ret[i] <- ui
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
    #' @param ... Unused.
    variance = function(...) {
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
