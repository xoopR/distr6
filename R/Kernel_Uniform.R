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
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(decorators = NULL, bw = 1) {

      private$.parameters <- getParameterSet(self, bw)
      self$setParameterValue(bw = bw)

      super$initialize(
        decorators = decorators,
        support = Reals$new()
      )
    },

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
          if (abs(xi_h) >= 2) {
            ret[i] <- 0
          } else {
            ret[i] <- (1 / 4) * (2 - (abs(xi_h)))
          }
        } else{
          if (xi_h >= 0 & xi_h <= 2) {
            if (ui_h >= 1) {
              ret[i] <- 1 / 4 * (2 - xi_h)
            }
            else if (ui_h >= (xi_h - 1) & ui_h <= 1) {
              ret[i] <- 1 / 4 * (ui_h - xi_h + 1)
            }
            else if (ui_h <= xi_h - 1) {
              ret[i] <- 0
            }
          } else if (xi_h >= -2 & xi_h <= 0) {
            if (ui_h >= (xi_h + 1)) {
              ret[i] <- 1 / 4 * (2 + xi_h)
            }
            else if (ui_h >= -1 & ui_h <= (xi_h + 1)) {
              ret[i] <- 1 / 4 * (ui_h + 1)
            }
            else if (ui_h <= -1) {
              ret[i] <- 0
            }
          } else {ret[i] <- 0}
        }
      }
      return(ret * 1 / self$getParameterValue("bw"))
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
          if (ui_h <= - 1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= 1) {
            ret[i] <-  (xi_h^3 + 2 * ui_h^3 + 3 * ui_h^2 * (2 - xi_h) +
                          6 * ui_h * (1 - xi_h) - 3 * xi_h + 2) / 24
          } else if (ui_h >= 1 & ui_h <= xi_h + 1) {
            ret[i] <- (xi_h^3 + 6 * ui_h^2 + 12 * ui_h - 12 * ui_h * xi_h - 2) / 24
          } else if (ui_h >= xi_h + 1) {
            ret[i] <- (xi_h^3 - 6 * xi_h^2 - 12 * xi_h + 24 * ui_h - 8) / 24
          }
        } else if (xi_h >= - 2 & xi_h <= 0) {
          if (ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= - 1) {
            ret[i] <- 0
          } else if (ui_h >= - 1 & ui_h <= xi_h + 1) {
            ret[i] <-  (2 * ui_h^3 + 3 * ui_h^2 * (2 - xi_h) +
                          6 * ui_h * (1 - xi_h) - 3 * xi_h + 2) / 24
          } else if (ui_h >= xi_h + 1 & ui_h <= 1) {
            ret[i] <- (- xi_h^3 + 6 * (ui_h^2 - xi_h^2) + 12 * (ui_h -  xi_h) - 2) / 24
          } else if (ui_h >= 1) {
            ret[i] <- (- xi_h^3 - 6 * xi_h^2 - 12 * xi_h + 24 * ui_h - 8) / 24
          }
        }
        else if (xi_h >= 2) {
          if (ui_h <=  -1) {
            ret[i] <- 0
          } else if (ui_h >= -1 & ui_h <= 1) {
            ret[i] <- 0
          } else if (ui_h >= 1 & ui_h <= xi_h - 1) {
            ret[i] <- 0
          } else if (ui_h >= xi_h - 1 & ui_h <= xi_h + 1) {
            ret[i] <- ui_h^2 / 4 + (1 / 4) * ui_h * (2 - 2 * xi_h) +
                      (1 / 4) * (1 - 2 * xi_h + xi_h^2)
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
            ret[i] <- 1 / 4 + ui_h / 2 + ui_h^2 / 4
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
      return(1 / 3)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      bw <- self$getParameterValue("bw")
      return(as.numeric(C_UniformKernelPdf(x, bw, log)))
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      bw <- self$getParameterValue("bw")
      return(as.numeric(C_UniformKernelCdf(x, bw, lower.tail, log.p)))
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
