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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(decorators = NULL, bw = 1) {

      private$.parameters <- getParameterSet(self, bw)
      self$setParameterValue(bw = bw)

    },

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
        xi_h = xi / self$getParameterValue("bw")
        ui_h = ui / self$getParameterValue("bw")
        if (ui_h == Inf) {
          if (abs(xi_h) > 2) {
            ret[i] <- 0
          } else {
            ret[i] <- (9 / 16) * (-abs(xi_h)^5 + 20 * abs(xi_h)^3 -
                       40 * abs(xi_h)^2 + 32) / 30
          }
        } else{
          if (xi_h >= 0 & xi_h <= 2) {
            if (ui_h >= 1) {
              ret[i] <- 3 * (-xi_h^5 + 20 * xi_h^3 - 40 * xi_h^2 + 32) / 160
              } else if (ui_h >= (xi_h - 1) & ui_h <= 1) {
              ret[i] <- (3 / 160) * (- xi_h^5 + 20 * xi_h^3 +
                                       10 * (xi_h^2) * (ui_h^3 - 3 * ui_h - 2) -
                        15 * xi_h * (ui_h^2 - 1)^2 +
                          6 * ui_h^5 - 20 * ui_h^3 + 30 * ui_h + 16)
            } else if (ui_h <= xi_h - 1) {
                ret[i] <- 0
            }
          } else if (xi_h >= -2 & xi_h <= 0) {
            if (ui_h >= (xi_h + 1)) {
              ret[i] <- 3 * (xi_h^5 - 20 * xi_h^3 - 40 * xi_h^2 + 32) / 160
              } else if (ui_h >= -1 & ui_h <= (xi_h + 1)) {
                ret[i] <- (3 / 160) * (10 * xi_h^2 * (ui_h^3 - 3 * ui_h - 2) -
                        15 * xi_h * (ui_h^2 - 1)^2 + 6 * ui_h^5 -
                        20 * ui_h^3 + 30 * ui_h + 16)
              } else if (ui_h <= -1) {
                  ret[i] <- 0
                  }
                }
              }
          }
      return(ret / self$getParameterValue("bw"))
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
        xi_h = xi / self$getParameterValue("bw")
        ui_h = ui / self$getParameterValue("bw")

        if (xi_h >= 0 & xi_h <= 2) {
          if (ui_h <= -1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= xi_h - 11) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= 1) {
            ret[i] <- (xi_h^7 - 42 * xi_h^5 + 105 * xi_h^3 + 84 * xi_h^2 -
                         280 * xi_h + 132) / 2240 +
              (ui_h * (280 * xi_h^3 - 840 * xi_h + 560)) / 2240 +
              (ui_h^2 * (210 * xi_h^3 - 420 * xi_h^2 - 630 * xi_h + 840)) / 2240 +
              (ui_h^4 * (-35 * xi_h^3 + 420 * xi_h - 140)) / 2240 +
              (ui_h^5 * (84 * xi_h^2 - 168)) / 2240 +
              (ui_h^3 * (-420 * xi_h^2 + 280 * xi_h + 420)) / 2240 -
              (ui_h^6 * xi_h) / 32 + ui_h^7 / 112
          } else if (ui_h >= 1 & ui_h <= xi_h + 1) {
            ret[i] <- (xi_h^7 - 42 * xi_h^5 + 560 * ui_h * xi_h^3 +
                         (168 - 840 * ui_h^2) * xi_h^2 +
                        (560 * ui_h^3 - 1680 * ui_h) * xi_h -
                         140 * ui_h^4 + 840 * ui_h^2 +
                        1120 * ui_h - 156) / 2240
          } else if (ui_h >= xi_h + 1) {
            ret[i] <- (xi_h^7 - 42 * xi_h^5 + 140 * xi_h^4 -
                         672 * xi_h^2 - 1120 * xi_h + 2240 * ui_h - 576) / 2240
          }
        } else if (xi_h >= -2 & xi_h <= 0) {
          if (ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= -1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= xi_h + 1) {
            ret[i] <- (ui_h * (280 * xi_h^3 - 840 * xi_h + 560)) / 2240 +
              (ui_h^2 * (210 * xi_h^3 - 420 * xi_h^2 - 630 * xi_h + 840)) / 2240 +
              (105 * xi_h^3 + 84 * xi_h^2 - 280 * xi_h + 132) / 2240 +
              (ui_h^4 * (-35 * xi_h^3 + 420 * xi_h - 140)) / 2240 +
              (ui_h^5 * (84 * xi_h^2 - 168)) / 2240 +
              (ui_h^3 * (-420 * xi_h^2 + 280 * xi_h + 420)) / 2240 -
              (ui_h^6 * xi_h) / 32 + ui_h^7 / 112
          } else if (ui_h >= xi_h + 1 & ui_h <= 1) {
            ret[i] <- - xi_h^7 / 2240 + (3 * xi_h^5) / 160 + xi_h^4 / 16 -
                      (3 * xi_h^2) / 10 - xi_h / 2 - ui_h^4 / 16 +
                      (3 * ui_h^2) / 8 + ui_h / 2 - 39 / 560
          } else if (ui_h >= 1) {
            ret[i] <- (- xi_h^7 + 42 * xi_h^5 + 140 * xi_h^4 -
                         672 * xi_h^2 - 1120 * xi_h + 2240 * ui_h - 576) / 2240
          }
        }  else if (xi_h >= 2) {
          if (ui_h <=  -1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= 1) {
            ret[i] <- 0
          } else if (ui_h >= 1 & ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= xi_h + 1) {
            ret[i] <- (- ((- xi_h + ui_h - 3) * (-xi_h + ui_h + 1)^3)) / 16
          } else if (ui_h >= xi_h + 1) {
            ret[i] <- ui_h - xi_h
          }
        } else if (xi_h <= -2) {
          if (ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= xi_h + 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h + 1 & ui_h <= -1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= 1) {
            ret[i] <- (1 / 16) * (3 + 8 * ui_h + 6 * ui_h^2 - ui_h^4)
          } else if (ui_h >= 1) {
            ret[i] <- ui_h
          }
        }
      }
      return(ret * self$getParameterValue("bw"))
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
