
#' @title Normal Kernel
#'
#' @description Mathematical and statistical functions for the NormalKernel kernel defined by the pdf,
#' \deqn{f(x) = exp(-x^2/2)/\sqrt{2\pi}}
#' over the support \eqn{x \in \R}{x \epsilon R}.
#'
#' @details We use the \code{erf} and \code{erfinv} error and inverse error functions from the Pracma
#' package.
#'
#' @name NormalKernel
#'
#' @section Constructor: NormalKernel$new(decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' }
#'
#' @inheritSection Kernel Public Variables
#' @inheritSection Kernel Public Methods
#'
#' @return Returns an R6 object inheriting from class Kernel.
#'
#' @export
NULL

NormalKernel <- R6Class("NormalKernel", inherit = Kernel, lock_objects = F,
  public = list(
    name = "NormalKernel",
    short_name = "Norm",
    description = "Normal Kernel",
    package = "pracma",

    initialize = function(decorators = NULL) {
      super$initialize(
        decorators = decorators,
        support = Reals$new()
      )
    },
    squared2Norm = function() {
      return((2 * sqrt(pi))^-1)
    },
    variance = function() {
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
      quantile <- numeric(p)
      if (log.p) {
        p = exp(p);
      }

      if (!lower.tail) {
        p = 1 - p;
      }

      quantile[p < 0 | p > 1] = NaN
      quantile[p == 0] = -Inf
      quantile[p == 1] = Inf
      quantile[p > 0 & p < 1] = sqrt(2) * pracma::erfinv(2 * p[p > 0 & p < 1] - 1)

      return(quantile)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels,
                         data.table::data.table(ShortName = "Norm", ClassName = "NormalKernel",
                                                Support = "\u211D", Packages = "pracma"))

