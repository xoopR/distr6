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

      # if (upper == Inf) {
      #   ret <- (1 / (2 * sqrt(pi))) * exp(- (x / 2)^2)
      # } else {
      #   ret <- exp(- (x^2) / 4) / (4 * sqrt(pi)) *
      #     (2 * pnorm((upper - x / 2) * sqrt(2)) - 1 + 1)}

      h = self$getParameterValue("bw")
      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]
        if (ui == Inf) {
          ret[i] <- (1 / (2 * sqrt(pi))) * exp(- (xi / (2 * h))^2) * 1 / h
        } else {
          ret[i] <- exp(- (xi^2) / (h^2 * 4)) / (4 * sqrt(pi)) *
            (2 * pnorm((ui / h  - xi / (2 * h)) * sqrt(2)) - 1 + 1) * 1 / h }
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
    },

    #' @description
    #' The integral of the cdf is defined by
    #' \deqn{\int_a^b (F_X(u)) du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    cdfIntegral = function(lower = -Inf, upper = Inf) {

      ll = length(lower)
      ul = length(upper)
      len = max(ll, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        li = lower[ifelse(i %% ll == 0, ll, i %% ll)] / self$getParameterValue("bw")
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)] / self$getParameterValue("bw")
        if (li == -Inf) {
            ret[i] <- (ui / 2) * (pracma::erf(ui / sqrt(2)) + 1) + (exp(- ui^2 / 2)) / (sqrt(2 * pi))
        } else {
          ret[i] <-  (ui * (pracma::erf(ui / sqrt(2)) + 1) - li * (pracma::erf(li / sqrt(2)) + 1)) / 2 -
            ((exp(ui^2 / (2)) - exp(li^2 / (2))) * exp(-(ui^2 + li^2) / (2))) / (sqrt(2 * pi))
        }
      }
      return(ret * self$getParameterValue("bw"))
    },

    #' @description
    #' The integral of the ccdf is defined by
    #' \deqn{\int_a^b (1  - F_X(u)) du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    ccdfIntegral = function (lower = -Inf, upper = Inf) {

      ll = length(lower)
      ul = length(upper)
      len = max(ll, ul)

      ret <- numeric(len)
      for (i in seq(len)) {
        li = lower[ifelse(i %% ll == 0, ll, i %% ll)] / self$getParameterValue("bw")
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)] / self$getParameterValue("bw")
        if (upper == Inf) {
          ret[i] <- (li / 2) * (pracma::erf(li / sqrt(2)) - 1)  + (exp(- li^2 / 2)) / (sqrt(2 * pi))
        } else {
          ret[i] <- ((exp(ui^2 / 2) - exp(li^2 / 2)) * exp(- (ui^2 + li^2) / 2)) / (sqrt(2 * pi)) -
            (uh * (pracma::erf(ui / sqrt(2)) - 1) - li * (pracma::erf(li / sqrt(2)) - 1)) /2
        }
      }
      return(ret * self$getParameterValue("bw"))
    },

    #' @description
    #' The energy Brier function is defined by
    #' \deqn{E[|X - X']}
    #' where \eqn{E|X|} is the mean aboslute deviation of distribution X.
    energyBrier = function (x) {

      len <- length(x)
      if (len == 1) {
          ret <- 2 /sqrt(pi) * self$getParameterValue("bw")
      } else {
        x_vec <- sapply(x, function (x, y) (x - y), y = x)
        bw_vec <- sapply(rep(self$getParameterValue("bw"), len), function (x, y) sqrt(x^2 + y^2),
                         y = rep(self$getParameterValue("bw"), len))
        ret <- x_vec * pracma::erf(x_vec/ (sqrt(2) * bw_vec)) + bw_vec * sqrt(2 / pi) * exp(-1/2 * (x_vec / bw_vec)^2)
        }
      return(ret)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      bw <- self$getParameterValue("bw")
      return(as.numeric(C_NormalKernelPdf(x, bw, log)))
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- 1 / 2 * (pracma::erf(x / (sqrt(2) * self$getParameterValue("bw"))) + 1)
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
