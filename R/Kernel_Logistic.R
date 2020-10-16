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
    initialize = function(decorators = NULL) {
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
      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]
        if (ui == Inf) {
          if (xi == 0) {
            ret[i] <- 1 / 6
          } else {
            ret[i] <- ((xi - 2) * exp(2 * xi) + (xi + 2) * exp(xi)) /
              (exp(3 * xi) - 3 * exp(2 * xi) + 3 * exp(xi) - 1)
          }
        } else {
          if (xi == 0) {
            ret[i] <- ((exp(ui) + 3) * exp((2 * ui))) / (6 * (exp(ui) + 1)^3)
          } else {
            ret[i] <- (exp(xi) *
                         (exp(2 * xi + ui) * log(exp(xi) + exp(ui)) +
                            exp(xi + 2 * ui) * log(exp(xi) + exp(ui)) +
                            2 * exp(xi + ui) * log(exp(xi) + exp(ui)) +
                            exp(2 * xi) * log(exp(xi) + exp(ui)) +
                            exp(xi) * log(exp(xi) + exp(ui)) +
                            exp(2 * ui) * log(exp(xi) + exp(ui)) +
                            exp(ui) * log(exp(xi) + exp(ui)) +
                            exp(2 * xi + ui) + 2 * exp(xi + 2 * ui) +
                            (- exp(ui) - 1) * xi * (exp(xi) + 1) *
                            (exp(xi) + exp(ui)) +
                            (- exp(ui) - 1) * log(exp(ui) + 1) * (exp(xi) + 1) *
                            (exp(xi) + exp(ui)) - 2 * exp(2 * ui) - exp(ui))) /
                            ((exp(ui) + 1) * (exp(xi) - 1)^3 * (exp(xi) + exp(ui)))}
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
        if (xi == 0) {
        ret[i] <- log(1 + exp(-ui)) + ui -  exp(ui) / (exp(ui) + 1)
        } else {
          ret[i] <- (exp(xi) * log((exp(ui) + exp(xi)) / exp(xi)) -
                       log(exp(ui) + 1)) / (exp(xi) - 1)
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
