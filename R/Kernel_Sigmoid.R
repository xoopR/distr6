#' @title Sigmoid Kernel
#'
#' @description Mathematical and statistical functions for the Sigmoid kernel defined by the pdf,
#' \deqn{f(x) = 2/\pi(exp(x) + exp(-x))^{-1}}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @template param_decorators
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Sigmoid <- R6Class("Sigmoid",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Sigmoid",
    short_name = "Sigm",
    description = "Sigmoid Kernel",

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
            ret[i] <- 2 / (pi^2)
          } else {
            ret[i] <- (4 * xi * exp(xi)) / (pi^2 * (exp(2 * xi) - 1))
          }
        } else {
          if (xi == 0) {
            ret[i] <- (1 + tanh(ui)) / pi^2
          } else{
          ret[i] <- (- (2 * exp(xi) * (log(exp(2 * xi) +
                                              exp(2 * ui)) - 2 * xi -
                                              log(exp(2 * ui) + 1)))) /
                                        ((pi^2) * (exp(xi) - 1) * (exp(xi) + 1))
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
      return(pi^2 / 4)
    }
  ),

  private = list(
    .isCdf = 0L,
    .isQuantile = 0L,
    .pdf = function(x, log = FALSE) {
      C_SigmoidKernelPdf(x, log)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Sigm", ClassName = "Sigmoid",
    Support = "\u211D", Packages = "-"
  )
)
