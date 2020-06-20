#' @title Silverman Kernel
#'
#' @description Mathematical and statistical functions for the Silverman kernel defined by the pdf,
#' \deqn{f(x) = exp(-|x|/\sqrt{2})/2 * sin(|x|/\sqrt{2} + \pi/4)}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @template param_decorators
#' @template class_kernel
#' @template class_distribution
#' @template method_pdfsquared2Norm
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @export
Silverman <- R6Class("Silverman",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Silverman",
    short_name = "Silv",
    description = "Silverman Kernel",

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

      cond1 <- (exp(-x / sqrt(2)) * (3 * sin(x / sqrt(2)) + 3 * cos(x / sqrt(2)))) / 2^(7 / 2) +
        (x * exp(-x / sqrt(2)) * sin(x / sqrt(2))) / 8
      cond2 <- (exp(x / sqrt(2)) * (-3 * sin(x / sqrt(2)) + 3 * cos(x / sqrt(2)))) / 2^(7 / 2) +
        (x * exp(x / sqrt(2)) * sin(x / sqrt(2))) / 8

      return(ifelse(x > 0, cond1, cond2))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(0)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_SilvermanKernelPdf(x, log)
    }
  )
)
.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Silv", ClassName = "Silverman",
    Support = "\u211D", Packages = "-"
  )
)
