#' @title Logistic Kernel
#'
#' @description Mathematical and statistical functions for the LogisticKernel kernel defined by the
#'  pdf, \deqn{f(x) = (exp(x) + 2 + exp(-x))^{-1}}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @name LogisticKernel
#' @template class_distribution
#' @template class_kernel
#' @template param_decorators
#' @template method_pdfsquared2Norm
#'
#' @export
LogisticKernel <- R6Class("LogisticKernel",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "LogisticKernel",
    short_name = "Logis",
    description = "Logistic Kernel",

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

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {
      h = self$getParameterValue("bw")
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
          if (xi_h == 0) {
            ret[i] <- 1 / 6
          } else {
            ret[i] <- (((xi_h - 2) * exp(2 * xi_h) + (xi_h + 2) * exp(xi_h)) /
              (exp(3 * xi_h) - 3 * exp(2 * xi_h) + 3 * exp(xi_h) - 1)) * 1 / h
          }
        } else {
          if (xi_h == 0) {
            ret[i] <- ((exp(ui_h) + 3) * exp((2 * ui_h))) / (6 * (exp(ui_h) + 1)^3)
          } else {
            ret[i] <- (exp(xi_h) *
                         (exp(2 * xi_h + ui_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(xi_h + 2 * ui_h) * log(exp(xi_h) + exp(ui_h)) +
                            2 * exp(xi_h + ui_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(2 * xi_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(xi_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(2 * ui_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(ui_h) * log(exp(xi_h) + exp(ui_h)) +
                            exp(2 * xi_h + ui_h) + 2 * exp(xi_h + 2 * ui_h) +
                            (- exp(ui_h) - 1) * xi_h * (exp(xi_h) + 1) *
                            (exp(xi_h) + exp(ui_h)) +
                            (- exp(ui_h) - 1) * log(exp(ui_h) + 1) * (exp(xi_h) + 1) *
                            (exp(xi_h) + exp(ui_h)) - 2 * exp(2 * ui_h) - exp(ui_h))) /
                            ((exp(ui_h) + 1) * (exp(xi_h) - 1)^3 * (exp(xi_h) + exp(ui_h)))}
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

        if (xi_h == 0) {
        ret[i] <- log(1 + exp(-ui_h)) + ui_h -  exp(ui_h) / (exp(ui_h) + 1)
        } else {
          ret[i] <- (exp(xi_h) * log((exp(ui_h) + exp(xi_h)) / exp(xi_h)) -
                       log(exp(ui_h) + 1)) / (exp(xi_h) - 1)
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
      return(pi^2 / 3)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_LogisticKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_LogisticKernelCdf(x, lower.tail, log.p)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      C_LogisticKernelQuantile(p, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Logis", ClassName = "LogisticKernel",
    Support = "\u211D", Packages = "-"
  )
)
