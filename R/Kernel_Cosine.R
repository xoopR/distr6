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

      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]
        if (abs(xi >= 2)) {
          ret[i] <- 0
        } else if (xi >= 0 & xi <= 2) {
          if (ui == Inf | ui >= 1) {
            ret[i] <- (pi / 32) * (2 * sin((pi * xi) / 2) - (xi - 2) * pi * cos(pi * xi / 2))
          } else if (ui >= (xi - 1) & ui <= 1) {
            ret[i] <- (pi) / (32) * (-sin(((pi) * xi - 2 * pi * ui) / (2)) +
                                     sin(((pi) * xi) / (2)) -
                                     (pi * xi - pi * ui - pi) * cos((pi * xi) / (2)))
          } else if (ui <= xi - 1) {
            ret[i] <- 0
          }
        } else if (xi <= 0 & xi >= -2) {
          if (ui == Inf | ui >= xi + 1) {
            ret[i] <- (pi) / (32) * (-2 * sin((pi * xi) / (2)) +
                                       (xi + 2) * pi * cos((pi * xi) / (2)))
          } else if (ui <= xi + 1 & ui >= -1) {
            ret[i] <- (pi) / (32) * (-sin((pi * xi - 2 * pi * ui) / (2)) +
                                       sin((pi * xi) / (2)) +
                                    (pi * ui + pi) * cos((pi * xi) / (2)))
          } else if (ui <= -1) {
            ret[i] <- 0
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
          } else if (ui >= -1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= 1) {
            ret[i] <-  (1 / (8 * pi)) *
              (2 * pi + 2 * ui * pi - 2 * xi * pi - 4 * cos((ui * pi) / 2) +
                                         (1 + ui - xi) * pi * cos((xi * pi) / 2) -
                      4 * cos((1 / 2) * (-ui + xi) * pi) +
                      3 * sin((xi * pi) / 2) + sin((1 / 2) * (-2 * ui + xi) * pi))
          } else if (ui >= 1 & ui <= xi + 1) {
            ret[i] <- (2 * (sin((pi * xi) / 2) - cos((pi * (xi - ui)) / 2))) / pi -
                      sin((pi * xi) / 2) / (4 * pi) -
                      ((xi - 2) * (cos((pi * xi) / 2) + 2)) / 8 + ui - 1 / 2
          } else if (ui >= xi + 1) {
            ret[i] <- (6 * sin((pi * xi) / 2) - pi * ((xi - 2) * cos((pi * xi) / 2) +
                                                          6 * xi - 8 * ui + 4)) / (8 * pi)
          }
        } else if (xi >= -2 & xi <= 0) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= - 1) {
            ret[i] <- 0
          } else if (ui >= - 1 & ui <= xi + 1) {
            ret[i] <- (-4 * cos((pi * (xi - ui)) / 2) +
                         sin((pi * (xi - 2 * ui)) / 2) -
                         3 * sin((pi * xi) / 2) + pi * (ui + 1) *
                     cos((pi * xi) / 2) - 4 * cos((pi * ui) / 2) +
                       2 * pi * ui + 2 * pi) / (8 * pi)
          } else if (ui >= xi + 1 & ui <= 1) {
            ret[i] <- - (6 * sin((pi * xi) / 2) -
                          pi * ((xi + 2) * cos((pi * xi) / 2) -
                                  2 * xi + 4 * ui) +
                       8 * cos((pi * ui) / 2)) / (8 * pi)
          } else if (ui >= 1) {
            ret[i] <- (sin((pi * xi) / 2) / pi + ((xi + 2) * cos((pi * xi) / 2)) / 2 +
                         xi + 2) / 4 -
                         sin((pi * xi) / 2) / pi - xi / 2 + ui - 1
          }
        } else if (xi >= 2) {
          if (ui <=  -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- 0
          } else if (ui >= 1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- (- (2 * cos((pi * (xi - ui)) / 2)) / pi - xi + ui + 1) / 2
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
            ret[i] <- (- (2 * cos((pi * ui) / 2)) / pi + ui + 1) / 2
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
      C_CosineKernelQuantile(p, lower.tail, log.p)
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
