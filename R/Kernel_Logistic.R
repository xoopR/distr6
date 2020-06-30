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
    pdfSquared2Norm = function(x = 0) {
      return(ifelse(x == 0, 1 / 6, ((x - 2) * exp(2 * x) + (x + 2) * exp(x)) /
        (exp(3 * x) - 3 * exp(2 * x) + 3 * exp(x) - 1)))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
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
