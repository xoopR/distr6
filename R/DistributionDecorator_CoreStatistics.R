#' @title Core Statistical Methods Decorator
#'
#' @template class_decorator
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_mgfcf
#' @template method_pgf
#'
#' @description This decorator adds numeric methods for missing analytic expressions in
#' [Distribution]s as well as adding generalised expectation and moments functions.
#'
#' @examples
#' decorate(Exponential$new(), "CoreStatistics")
#' Exponential$new(decorators = "CoreStatistics")
#' CoreStatistics$new()$decorate(Exponential$new())
#' @export
CoreStatistics <- R6Class("CoreStatistics",
  inherit = DistributionDecorator,
  public = list(
    #' @description
    #' Numerically estimates the moment-generating function.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    mgf = function(t, ...) {
      self$genExp(trafo = function(x) exp(x * t), ...)
    },

    #' @description
    #' Numerically estimates the characteristic function.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    cf = function(t, ...) {
      if (testDiscrete(self)) {
        return(self$genExp(trafo = function(x) exp(x * t * 1i), ...))
      } else {
        return(self$genExp(trafo = function(x) Re(exp(x * t * 1i)), ...) +
          1i * self$genExp(trafo = function(x) Im(exp(x * t * 1i)), ...))
      }
    },

    #' @description
    #' Numerically estimates the probability-generating function.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    pgf = function(z, ...) {
      if (testDiscrete(self)) {
        x <- self$genExp(trafo = function(x) z^x, ...)
        return(x)
      } else {
        return(NaN)
      }
    },

    #' @description
    #' Numerically estimates the entropy function.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    entropy = function(base = 2, ...) {
      message(.distr6$message_numeric)
      return(suppressMessages(self$genExp(trafo = function(x) -log(self$pdf(x), base), ...)))
    },

    #' @description
    #' Numerically estimates the distribution skewness.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    skewness = function(...) {
      return(self$kthmoment(k = 3, type = "standard", ...))
    },

    #' @description
    #' Numerically estimates the distribution kurtosis.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    kurtosis = function(excess = TRUE, ...) {
      kurtosis <- suppressMessages(self$kthmoment(k = 4, type = "standard", ...))
      if (testContinuous(self)) {
        message(.distr6$message_numeric)
      }
      if (excess) {
        return(kurtosis - 3)
      } else {
        return(kurtosis)
      }
    },

    #' @description
    #' Numerically estimates the distribution variance.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    variance = function(...) {
      if (testUnivariate(self)) {
        message(.distr6$message_numeric)
        return(suppressMessages(self$genExp(trafo = function(x) x^2, ...) - self$genExp(...)^2))
      }
    },

    #' @description
    #' The kth central moment of a distribution is defined by
    #' \deqn{CM(k)_X = E_X[(x - \mu)^k]}
    #' the kth standardised moment of a distribution is defined by
    #' \deqn{SM(k)_X = \frac{CM(k)}{\sigma^k}}{SM(k)_X = CM(k)/\sigma^k}
    #' the kth raw moment of a distribution is defined by
    #' \deqn{RM(k)_X = E_X[x^k]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param k `integer(1)` \cr
    #' The `k`-th moment to evaluate the distribution at.
    #' @param type `character(1)` \cr
    #' Type of moment to evaluate.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    kthmoment = function(k, type = c("central", "standard", "raw"), ...) {

      if (testUnivariate(self)) {

        type <- match.arg(type)

        if (type == "central") {
          if (k == 0) {
            return(1)
          }
          if (k == 1) {
            return(0)
          }
        }

        message(.distr6$message_numeric)

        if (type == "raw") {
          suppressMessages(return(self$genExp(trafo = function(x) x^k, ...)))
        }

        centralMoment <- suppressMessages(self$genExp(trafo = function(x) (x - self$genExp(...))^k,
                                                      ...))

        if (type == "central") {
          return(centralMoment)
        } else if (type == "standard") {
          suppressMessages(return(centralMoment / self$stdev()^k))
        }
      }
    },

    #' @description
    #' Numerically estimates \eqn{E[f(X)]} for some function \eqn{f}.
    #' @param trafo `function()` \cr
    #' Transformation function to define the expectation, default is distribution mean.
    #' @param cubature `logical(1)` \cr
    #' If `TRUE` uses [cubature::cubintegrate] for approximation, otherwise [integrate].
    #' @param ... `ANY` \cr
    #' Passed to [cubature::cubintegrate].
    genExp = function(trafo = NULL, cubature = FALSE, ...) {
      if (is.null(trafo)) {
        trafo <- function() {
          return(x)
        }
        formals(trafo) <- alist(x = ) # nolint
      }

      support <- private$.properties$support
      count <- support$properties$countability
      inf <- support$lower
      sup <- support$upper

      if (count != "uncountable") {
        ws <- self$workingSupport()
        rng <- seq.int(ws$lower, ws$upper)
        pdfs <- self$pdf(rng)
        xs <- trafo(rng)
        xs[pdfs == 0] <- 0
        return(sum(pdfs * xs))
      } else {
        message(.distr6$message_numeric)
        if (cubature) {
          requireNamespace("cubature")
          return(suppressMessages(cubature::cubintegrate(function(x) {
            pdfs <- self$pdf(x)
            xs <- trafo(x)
            xs[pdfs == 0] <- 0
            return(xs * pdfs)
          }, lower = inf,
          upper = sup,
          ...)$integral))
        } else {
          return(suppressMessages(integrate(function(x) {
            pdfs <- self$pdf(x)
            xs <- trafo(x)
            xs[pdfs == 0] <- 0
            return(xs * pdfs)
          }, lower = inf, upper = sup)$value))
        }
      }
    },

    #' @description
    #' Numerically estimates the distribution mode.
    mode = function(which = "all") {
      if (private$.isRand) {
        return(modal(round(self$rand(1e5), 4)))
      } else {
        lower <- ifelse(self$inf == -Inf, -1e3, self$inf)
        upper <- ifelse(self$sup == Inf, 1e3, self$sup)

        if (testDiscrete(self)) {
          return((self$inf:self$sup)[which.max(self$pdf(self$inf:self$sup))])
        } else {
          return(optimize(self$pdf, interval = c(lower, upper), maximum = T)$maximum)
        }
      }
    },

    #' @description
    #' Numerically estimates the distribution mean.
    #' @param ... `ANY` \cr
    #' Passed to `$genExp`.
    mean = function(...) {
      return(self$genExp(...))
    }
  )
)

.distr6$decorators <- append(.distr6$decorators, list(CoreStatistics = CoreStatistics))
