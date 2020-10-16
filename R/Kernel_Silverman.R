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
  inherit = Kernel,
  lock_objects = FALSE,
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
    pdfSquared2Norm = function(x = 0, upper = Inf) {

      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {

        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]

        if (xi >= 0) {
          if (ui == Inf) {
            ret[i] <- (exp(-xi / sqrt(2)) * (3 * sin(xi / sqrt(2)) + 3 *
                                                cos(xi / sqrt(2)))) / 2^ (7 / 2) +
              (xi * exp(-xi / sqrt(2)) * sin(xi / sqrt(2))) / 8
          } else if (ui <= 0) {
            ret[i] <- (exp((2 * ui - xi) / sqrt(2)) *
                         (2 * cos(xi / sqrt(2)) - sin((2 * ui - xi) / sqrt(2)) +
                            cos((2 * ui - xi) / sqrt(2)))) / 2^ (9 / 2)
          } else if (ui >= 0 & ui <= xi) {
            ret[i] <- (exp(-xi / sqrt(2)) * ((2 * ui + sqrt(2)) * sin(xi / sqrt(2)) +
                                            sqrt(2) * sin((2 * ui - xi) / sqrt(2)))) / 16 +
              (exp(- xi / sqrt(2)) * (sin(xi / sqrt(2)) + 3 * cos(xi / sqrt(2)))) / 2^ (9 / 2)
          } else if (ui >= xi) {
            ret[i] <- (exp(- xi  / sqrt(2)) / (8 * sqrt(2))) *
              ((3 + sqrt(2) * xi) * sin(xi / sqrt(2)) + 3 * cos(xi / sqrt(2))) +
              (exp((xi - 2 * ui) / sqrt(2)) / (16 * sqrt(2))) *
              (sin((xi - 2 * ui) / sqrt(2)) -
                 cos((xi - 2 * ui) / sqrt(2)) - 2 * cos(xi / sqrt(2)))
          }
        } else if (xi <= 0) {
          if (ui == Inf) {
            ret[i] <- (exp(xi / sqrt(2)) * (-3 * sin(xi / sqrt(2)) + 3 *
                                               cos(xi / sqrt(2)))) / 2^ (7 / 2) +
              (xi * exp(xi / sqrt(2)) * sin(xi / sqrt(2))) / 8
          } else if (ui <= xi) {
            ret[i] <- (exp((2 * ui - xi) / sqrt(2)) / (16 * sqrt(2))) *
              (sin((xi - 2 * ui) / sqrt(2)) + cos((xi - 2 * ui) / sqrt(2)) +
                 2 * cos(xi / sqrt(2)))
          } else if (ui >= xi & ui <= 0) {
            ret[i] <- (- (exp(xi / sqrt(2)) * (2 * sin((xi - 2 * ui) / sqrt(2)) -
                      (2^ (3 / 2) * (xi - ui) - 3) * sin(xi / sqrt(2)) -
                      3 * cos(xi / sqrt(2))))) / 2^ (9 / 2)
          } else if (ui >= 0) {
            ret[i] <- (exp((xi - 2 * ui) / sqrt(2)) *
                         (sin((xi - 2 * ui) / sqrt(2)) -
                            cos((xi - 2 * ui) / sqrt(2)) +
                            exp(sqrt(2) * ui) *
                            (2^ (3 / 2) * xi - 6) * sin(xi / sqrt(2)) +
                    2 * (3 * exp(sqrt(2) * ui) - 1) * cos(xi / sqrt(2)))) / 2^ (9 / 2)
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

        if (xi >= 0) {
          if (ui >= -Inf & ui <= 0) {
            ret[i] <- (exp((2 * ui - xi) / sqrt(2)) *
                         (2 * cos(xi / sqrt(2)) + sin((2 * ui - xi) / sqrt(2)) +
                      cos((2 * ui - xi) / sqrt(2)))) / (2^ (9 / 2))
          } else if (ui >= 0 & ui <= xi) {
            ret[i] <- (exp(- xi / sqrt(2)) *
                         (sqrt(2) * (sin((xi - 2 * ui) / sqrt(2)) +
                                       3 * sin(xi / sqrt(2))) -
                            2 * (ui + 2^ (3 / 2)) * cos(xi / sqrt(2)) +
                            2^ (5 / 2) * exp(ui / sqrt(2)) *
                            (sin((ui - xi) / sqrt(2)) +
                               cos((ui - xi) / sqrt(2))))) / 16 +
              (exp(- xi / sqrt(2)) * (3 * cos(xi / sqrt(2)) -
                                          sin(xi / sqrt(2)))) / (2^ (9 / 2))
          } else if (ui >= xi) {
            ret[i] <- (8 * exp((xi - ui) / (sqrt(2))) *
                         (sin((xi - ui) / (sqrt(2))) +
                     cos((xi - ui) / (sqrt(2)))) -
                       exp((xi - 2 * ui) / (sqrt(2))) *
                       (sin((xi - 2 * ui) / (sqrt(2))) +
                     cos((xi - 2 * ui) / (sqrt(2))) +
                       2 * cos((xi) / (sqrt(2))))) / (16 * sqrt(2)) -
                    (8 * exp((-ui) / (sqrt(2))) * (sin((ui) / (sqrt(2))) -
                    cos((ui) / (sqrt(2))))) / (16 * sqrt(2)) +
              (exp((- xi) / (sqrt(2))) * (10 * sin((xi) / (sqrt(2))) -
                    (10 + 2 * sqrt(2) * xi) * cos((xi) / (sqrt(2)))) -
                 16 * sqrt(2) * (xi - ui)) / (16 * sqrt(2))
          }
        } else if (xi <= 0) {
          if (ui <= xi) {
            ret[i] <-  (exp((2 * ui - xi) / sqrt(2)) * (2 * cos(xi / sqrt(2)) +
                      sin((2 * ui - xi) / sqrt(2)) +
                        cos((2 * ui - xi) / sqrt(2)))) / (2^ (9 / 2))
          } else if (ui >= xi  & ui <= 0) {
            ret[i] <- (exp(xi / sqrt(2)) *
                         (sqrt(2) * (sin((xi - 2 * ui) / sqrt(2)) -
                                       3 * sin(xi / sqrt(2))) -
                            2 * (- xi + ui + 2^ (3 / 2)) *
                            cos(xi / sqrt(2))) +
                         2^ (5 / 2) * exp(ui / sqrt(2)) * (sin(ui / sqrt(2)) +
                  cos(ui / sqrt(2)))) / 16 +
              (exp(xi / sqrt(2)) * (sin(xi / sqrt(2)) +
                                        3 * cos(xi / sqrt(2)))) / (2^ (9 / 2))
          } else if (ui >= 0) {
            ret[i] <- (exp(- ui / sqrt(2)) *
                        (exp(ui / sqrt(2)) *
                           (256 * exp((xi - ui) / sqrt(2)) *
                              (sin((xi - ui) / sqrt(2)) +
                                 cos((xi - ui) / sqrt(2))) -
                              32 * exp((xi - 2 * ui) / sqrt(2)) *
                              sin((xi - 2 * ui) / sqrt(2)) -
                              32 * exp((xi - 2 * ui) / sqrt(2)) *
                              cos((xi - 2 * ui) / sqrt(2)) -
                              320 * exp(xi / sqrt(2)) * sin(xi / sqrt(2)) -
                              64 * exp((xi - 2 * ui) / sqrt(2)) * cos(xi / sqrt(2)) +
                              2^ (13 / 2) * xi * exp(xi /  sqrt(2)) * cos(xi / sqrt(2)) -
                              320 * exp(xi / sqrt(2)) * cos(xi / sqrt(2)) +
                              2^ (19 / 2) * ui) - 256 * sin(ui / sqrt(2)) +
                              256 * cos(ui / sqrt(2)))) / 2 ^ (19 / 2)
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
      return(0)
    }
  ),

  private = list(
    .isQuantile = 0L,
    .pdf = function(x, log = FALSE) {
      C_SilvermanKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_SilvermanKernelCdf(x, lower.tail, log.p)
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
