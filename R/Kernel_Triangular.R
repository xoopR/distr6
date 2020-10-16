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
      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {

        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]

        if (xi >= 0 & xi <= 1) {
          if (ui >= (xi - 1) & ui <= 0) {
            ret[i] <- (xi^3 + (- 3 * ui^2 - 6 * ui - 3) *
                         xi + 2 * ui^3 + 6 * ui^2 + 6 * ui + 2) / 6
          } else if (ui >= 0 & ui <= xi) {
            ret[i] <- (- 2 * ui^3 + 3 * ui^2 * xi -
                         6 * ui * xi + 6 * ui + xi^3 - 3 * xi + 2) / 6
          } else if (ui >= xi & ui <= 1) {
            ret[i] <- (2 * ui^3 - 3 * ui^2 * xi -
                         6 * ui^2 + 6 * ui * xi + 6 * ui +
                        3 * xi^3 - 6 * xi^2 - 3 * xi + 2) / 6
          } else if (ui == Inf | ui > 1) {
            ret[i] <- (3 * xi^3 - 6 * xi^2 + 4) / 6
          } else {
            ret[i] <- 0
            }
        } else if (xi >=  1 & xi <= 2) {
          if (ui == Inf | ui >= 1) {
            ret[i] <- (- xi^3 + 6 * xi^2 - 12 * xi + 8) / 6
          } else if (ui >= (xi - 1) & ui <= 1) {
            ret[i] <- (- xi^3 + 6 * xi^2 - (- 3 * ui^2 + 6 * ui + 9) *
                         xi - 2 * ui^3 + 6 * ui + 4) / 6
          } else {
            ret[i] <- 0
            }
        } else if (xi >= -1 & xi <= 0) {
          if (ui == Inf | ui >= xi + 1) {
            ret[i] <- (- xi^3 + 6 * xi^2 + 4) / 6
          } else if (ui >= - 1 & ui <= xi) {
            ret[i] <- (- (3 * ui ^2 + 6 * ui  + 3) * xi +
                         2 * ui^3 + 6 * ui ^2 + 6 * ui + 2) / 6
          } else if (ui >= xi & ui <= 0) {
            ret[i] <- (- (xi * (2 * xi^2 + 6 * xi -
                                   3 * ui * (ui + 2) + 3) +
                         2 * (ui^3 - 3 * ui - 1))) / 6
          } else if (ui >= 0 & ui <= xi + 1) {
            ret[i] <- (2 * ui^3 - 3 * ui^2 * xi -
                         6 * ui^2 + 6 * ui * xi + 6 * ui - 2 * xi^3 -
                        6 * xi^2 - 3 * xi + 2) / 6
          } else {
            ret[i] <- 0
            }
        } else if (xi >= - 2 & xi <= - 1) {
          if (ui == Inf | ui >= xi + 1) {
            ret[i] <- (xi^3 + 6 * xi^2 + 12 * xi + 8) / 6
          } else if (ui >= -1 & ui <= xi + 1) {
            ret[i] <- ((3 * ui^2 + 6 * ui) * xi -
                         2 * ui^3 + 6 * ui + 3 * xi + 4) / 6
          }
        } else {
          ret[i] <- 0
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

          if (xi >= 0 & xi <= 1) {
            if (ui <= -1) {
              ret[i] <- 0
            } else if (ui >= -1 & ui <= xi - 1) {
              ret[i] <- 0
            } else if (ui >= xi - 1 & ui <= 0) {
              ret[i] <- (- xi^5 + 10 * xi^2 - 15 * xi + 6) / 120 +
                (ui * (30 * xi^2 - 60 * xi + 30)) / 120 +
                (ui^2 * (30 * xi^2 - 90 * xi + 60)) / 120 +
                (ui^3 * (10 * xi^2 - 60 * xi + 60)) / 120 +
                (ui^4 * (30 - 15 * xi)) / 120 +
                ui^5 / 20
            } else if (ui >= 0 & ui <= xi) {
              ret[i] <- (-xi^5 / 120) + (ui * (30 * xi^2 - 60 * xi + 30)) / 120 +
                (ui^2 * (30 * xi^2 - 90 * xi + 60)) / 120 + xi^2 / 12 +
                (ui^3 * (-10 * xi^2 - 20 * xi + 40)) / 120 +
                (ui^4 * xi) / 8 - xi / 8 - ui^5 / 20 + 1 / 20
            } else if (ui >= xi & ui <= 1) {
              ret[i] <- (-xi^5 / 60) + (- xi^5 + 40 * xi^3 - 30 * xi) / 120 +
                xi^4 / 12 - xi^3 / 6 + (ui^3 * (10 * xi^2 + 60 * xi + 20)) / 120 +
                xi^2 / 12 + (ui^2 * (- 30 * xi^2 - 30 * xi + 60)) / 120 +
                (ui * (-30 * xi^2 - 60 * xi + 30)) / 120 +
                xi / 8 + (ui^4 * (-15 * xi - 30)) / 120 +
                ui^5 / 20 + 1 / 20
            } else if (ui >= xi  & ui <= xi + 1) {
              ret[i] <- (-3 * xi^5 + 10 * xi^4 + 20 * xi^3 + 20 * xi^2 - 8) / 120 +
                (ui * (-60 * xi^2 - 120 * xi + 60)) / 120 +
                (ui^2 * (60 * xi + 60)) / 120 - ui^3 / 6
            } else if (ui >= xi + 1) {
              ret[i] <- (1 / 120) * (- 28 + 120 * ui - 60 * xi -
                                       40 * xi^2 + 10 * xi^4 - 3 * xi^5)
            }
          }  else if (xi >= 1 & xi <= 2) {
            if (ui <= -1) {
              ret[i] <- 0
            } else if (ui >= - 1 & ui <= xi - 1) {
              ret[i] <- 0
            } else if (ui >= xi - 1 & ui <= 0) {
              ret[i] <- (- (ui^5 / 20)) + (ui^4 * xi) / 8 +
                (1 / 120) * ui^3 * (40 - 20 * xi - 10 * xi^2) +
                (1 / 120) * ui^2 * (60 - 90 * xi + 30 * xi^2) +
                (1 / 120) * ui * (30 - 60 * xi + 30 * xi^2) +
                (1 / 120) * (4 - 5 * xi - 10 * xi^2 + 20 * xi^3 - 10 * xi^4 + xi^5)
            } else if (ui >= 0 & ui <= xi) {
              ret[i] <- (1 / 120) * (20 * ui^3 - 60 * ui^2 * (xi - 1) +
                                       60 * ui * (xi - 1)^2 + xi^5 -
                                       10 * xi^4 + 20 * xi^3 - 20 * xi^2 + 20 * xi - 12)
            } else if (ui >= xi & ui <= xi + 1) {
              ret[i] <- (xi^5 - 10 * xi^4 + 60 * xi^3 + (- 60 * ui - 20) * xi^2 +
                           (60 * ui^2 - 120 * ui + 20) * xi -
                           20 * ui^3 + 60 * ui^2 + 60 * ui - 12) / 120
            } else if (ui >= xi + 1) {
              ret[i] <- (xi^5 - 10 * xi^4 + 40 * xi^3 - 80 * xi^2 -
                           40 * xi + 120 * ui - 32) / 120
            }
          } else if (xi >= - 1 & xi <= 0) {
            if (ui <= xi - 1) {
              ret[i] <- 0
            } else if (ui >= xi - 1 & ui <= - 1) {
              ret[i] <- 0
            } else if (ui >= -1 & ui <= xi) {
              ret[i] <- ui^5 / 20 + (1 / 120) * ui^4 * (30 - 15 * xi) +
                (1 / 120) * ui^3 * (60 - 60 * xi + 10 * xi^2) +
                (1 / 120) * (6 - 15 * xi + 10 * xi^2) +
                (1 / 120) * ui^2 * (60 - 90 * xi + 30 * xi^2) +
                (1 / 120) * ui * (30 - 60 * xi + 30 * xi^2)
            } else if (ui >= xi & ui <= 0) {
              ret[i] <- (xi^5 + 10 * xi^4 + 20 * xi^3 - 30 * xi) / 120 +
                xi^5 / 120 + xi^2 / 12 + (ui^3 * (- 10 * xi^2 + 20 * xi + 40)) / 120 +
                (ui^2 * (- 30 * xi^2 - 30 * xi + 60)) / 120 +
                (ui * (- 30 * xi^2 - 60 * xi + 30)) / 120 +
                (ui^4 * xi) / 8 + xi / 8 - ui^5 / 20 + 1 / 20
            } else if (ui >= 0 & ui <= xi + 1) {
              ret[i] <- xi^5 / 60 + xi^4 / 12 + (xi^3)  / 6 +
                (ui * (- 30 * ui * (xi^2 + xi - 2) +
                10 * ui^2 * (xi * (xi + 6) + 2) -
                30 * xi * (xi + 2) - 15 * ui^3 * (xi + 2) +
                6 * ui^4 + 30)) / 120 + xi^2 / 12 - xi / 8 + 1 / 20
            } else if (ui >= xi + 1 & ui <= 1) {
              ret[i] <- (3 * xi^5 + 10 * xi^4 - 40 * xi^2 -
                           60 * xi - 20 * ui *
                           (ui^2 - 3 * ui - 3) - 8) / 120
            } else if (ui >= 1) {
              ret[i] <- (1 / 120) * (- 28 + 120 * ui - 60 * xi -
                                       40 * xi^2 + 10 * xi^4 + 3 * xi^5)
            }
          } else if (xi >= -2 & xi <= -1) {
            if (ui <= xi - 1) {
              ret[i] <- 0
            } else if (ui >= xi - 1 & ui <= - 1) {
              ret[i] <- 0
            } else if (ui >= - 1 & ui <= xi + 1) {
              ret[i] <- (ui^3 * (- 10 * xi^2 + 20 * xi + 40)) / 120 +
                (- 10 * xi^2 - 25 * xi + 4) / 120 +
                (ui^2 * (- 30 * xi^2 - 30 * xi + 60)) / 120 +
                (ui * (- 30 * xi^2 - 60 * xi + 30)) / 120 +
                (ui^4 * xi) / 8 - ui^5 / 20
            } else if (ui >= xi + 1 & ui <= 0) {
              ret[i] <- (- xi^5 - 10 * xi^4 - 40 * x [i]^3 - 80 * xi^2 -
                           80 * xi + 20 * ui^3 + 60 * ui^2 + 60 * ui - 12) / 120
            } else if (ui >= 0 & ui <= 1) {
              ret[i] <- (- xi^5 - 10 * xi^4 - 40 * xi^3 - 80 * xi^2 -
                           80 * xi - 20 * ui * (ui^2 - 3 * ui - 3) - 12) / 120
            } else if (ui >= 1) {
              ret[i] <- (- xi^5 - 10 * xi^4 - 40 * xi^3 - 80 * xi^2 -
                           80 * xi + 120 * ui - 32) / 120
            }
          } else if (xi >= 2) {
            if (ui <= xi - 1) {
              ret[i] <- 0
            } else if (ui >= xi - 1 & ui <= xi) {
              ret[i] <- (1 / 6) * (1 + ui - xi)^3
            } else if (ui >= xi & ui <= xi + 1) {
              ret[i] <- (xi^3 + (3 - 3 * ui) * xi^2 +
                           (3 * ui^2 - 6 * ui - 3) *
                           xi - ui^3 + 3 * ui^2 + 3 * ui) / 6 + 1 / 6
            } else if (ui >= xi + 1) {
              ret[i] <- ui - xi
            }
          } else if (xi <= -2) {
            if (ui <= -1) {
              ret[i] <- 0
            } else if (ui >= -1 & ui <= 0) {
              ret[i] <- (ui^3 + 3 * ui^2 + 3 * ui + 1) / 6
            } else if (ui >= 0 & ui <= 1) {
              ret[i] <- (- ui^3 + 3 * ui^2 + 3 * ui + 1) / 6
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
      C_TriangularKernelQuantile(p, lower.tail, log.p)
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
