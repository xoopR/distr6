#' @title Normal Kernel
#'
#' @description Mathematical and statistical functions for the NormalKernel kernel defined by
#' the pdf, \deqn{f(x) = exp(-x^2/2)/\sqrt{2\pi}} over the support \eqn{x \in \R}{x \epsilon R}.
#'
#' @details We use the \code{erf} and \code{erfinv} error and inverse error functions from
#' \CRANpkg{pracma}.
#'
#' @name NormalKernel
#' @template param_decorators
#' @template class_distribution
#' @template class_kernel
#' @template field_packages
#' @template method_pdfsquared2Norm
#'
#' @export
NormalKernel <- R6Class("NormalKernel",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "NormalKernel",
    short_name = "Norm",
    description = "Normal Kernel",
    packages = "pracma",

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
          ret[i] <- (1 / (2 * sqrt(pi))) * exp(- (xi / 2)^2)
        } else {
          ret[i] <- exp(- (xi^2) / 4) / (4 * sqrt(pi)) *
            (2 * pnorm((ui - xi / 2) * sqrt(2)) - 1 + 1)}
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
      return(1)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_NormalKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- 1 / 2 * (pracma::erf(x / sqrt(2)) + 1)
      if (!lower.tail) {
        cdf <- 1 - cdf
      }
      if (log.p) {
        cdf <- log(cdf)
      }

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      quantile <- numeric(length(p))
      if (log.p) {
        p <- exp(p)
      }

      if (!lower.tail) {
        p <- 1 - p
      }

      quantile[p < 0 | p > 1] <- NaN
      quantile[p == 0] <- -Inf
      quantile[p == 1] <- Inf
      quantile[p > 0 & p < 1] <- sqrt(2) * pracma::erfinv(2 * p[p > 0 & p < 1] - 1)

      return(quantile)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Norm", ClassName = "NormalKernel",
    Support = "\u211D", Packages = "pracma"
  )
)
