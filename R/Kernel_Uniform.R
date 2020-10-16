#' @title Uniform Kernel
#'
#' @description Mathematical and statistical functions for the Uniform kernel defined by the pdf,
#' \deqn{f(x) = 1/2}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name UniformKernel
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
UniformKernel <- R6Class("UniformKernel",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "UniformKernel",
    short_name = "Unif",
    description = "Uniform Kernel",

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
          if (abs(xi) >= 2) {
            ret[i] <- 0
          } else {
            ret[i] <- (1 / 4) * (2 - (abs(xi)))
          }
        } else{
          if (xi >= 0 & xi <= 2) {
            if (ui >= 1) {
              ret[i] <- 1 / 4 * (2 - xi)
            }
            else if (ui >= (xi - 1) & ui <= 1) {
              ret[i] <- 1 / 4 * (ui - xi + 1)
              }
            else if (ui <= xi - 1) {
              ret[i] <- 0
              }
          } else if (xi >= -2 & xi <= 0) {
            if (ui >= (xi + 1)) {
              ret[i] <- 1 / 4 * (2 + xi)
              }
            else if (ui >= -1 & ui <= (xi + 1)) {
              ret[i] <- 1 / 4 * (ui + 1)
              }
            else if (ui <= -1) {
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
          if (ui <= - 1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= 1) {
            ret[i] <-  (xi^3 + 2 * ui^3 + 3 * ui^2 * (2 - xi) +
                          6 * ui * (1 - xi) - 3 * xi + 2) / 24
          } else if (ui >= 1 & ui <= xi + 1) {
            ret[i] <- (xi^3 + 6 * ui^2 + 12 * ui - 12 * ui * xi - 2) / 24
          } else if (ui >= xi + 1) {
            ret[i] <- (xi^3 - 6 * xi^2 - 12 * xi + 24 * ui - 8) / 24
          }
        } else if (xi >= - 2 & xi <= 0) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= - 1) {
            ret[i] <- 0
          } else if (ui >= - 1 & ui <= xi + 1) {
            ret[i] <-  (2 * ui^3 + 3 * ui^2 * (2 - xi) +
                          6 * ui * (1 - xi) - 3 * xi + 2) / 24
          } else if (ui >= xi + 1 & ui <= 1) {
            ret[i] <- (- xi^3 + 6 * (ui^2 - xi^2) + 12 * (ui -  xi) - 2) / 24
          } else if (ui >= 1) {
            ret[i] <- (- xi^3 - 6 * xi^2 - 12 * xi + 24 * ui - 8) / 24
          }
        }
        else if (xi >= 2) {
          if (ui <=  -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- 0
          } else if (ui >= 1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- ui^2 / 4 + (1 / 4) * ui * (2 - 2 * xi) +
                      (1 / 4) * (1 - 2 * xi + xi^2)
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
            ret[i] <- 1 / 4 + ui / 2 + ui^2 / 4
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
      return(1 / 3)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_UniformKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_UniformKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_UniformKernelQuantile(p, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Unif", ClassName = "UniformKernel",
    Support = "[-1,1]", Packages = "-"
  )
)
