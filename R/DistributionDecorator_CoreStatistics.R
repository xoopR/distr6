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
      return(self$genExp(trafo = function(x) exp(x * t), ...))
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

      count <- self$properties$support$properties$countability
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
          }, lower = self$inf,
          upper = self$sup,
          ...)$integral))
        } else {
          return(suppressMessages(integrate(function(x) {
            pdfs <- self$pdf(x)
            xs <- trafo(x)
            xs[pdfs == 0] <- 0
            return(xs * pdfs)
          }, lower = self$inf, upper = self$sup)$value))
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

#' @title Moment Generating Function
#' @name mgf
#' @description Moment generating function of a distribution
#'
#' @usage mgf(object, t, ...)
#'
#' @param object Distribution.
#' @param t integer to evaluate moment generating function at.
#' @param ... Passed to `$genExp`.
#'
#' @return Moment generating function evaluated at t as a numeric.
#'
#' @export
NULL

#' @title Characteristic Function
#' @name cf
#' @description Characteristic function of a distribution
#'
#' @usage cf(object, t, ...)
#'
#' @param object Distribution.
#' @param t integer to evaluate characteristic function at.
#' @param ... Passed to `$genExp`.
#'
#' @return Characteristic function evaluated at t as a numeric.
#'
#' @export
NULL

#' @title Probability Generating Function
#' @name pgf
#' @description Probability generating function of a distribution
#'
#' @usage pgf(object, z, ...)
#'
#' @param object Distribution.
#' @param z integer to evaluate characteristic function at.
#' @param ... Passed to `$genExp`.
#'
#' @return Probability generating function evaluated at z as a numeric if distribution is discrete,
#' otherwise NaN.
#'
#' @export
NULL

#' @title Distribution Entropy
#' @name entropy
#' @description (Information) Entropy of a distribution
#'
#' @param object Distribution.
#' @param base base of the entropy logarithm, default = 2 (Shannon entropy)
#' @param ... Passed to `$genExp`.
#'
#' @usage entropy(object, base = 2, ...)
#'
#' @return Entropy with given base as a numeric.
#'
#' @export
NULL

#' @title Distribution Skewness
#' @name skewness
#' @description Skewness of a distribution
#'
#' @usage skewness(object, ...)
#'
#' @param object Distribution.
#' @param ... Passed to `$genExp`.
#'
#' @return Skewness as a numeric.
#'
#' @export
NULL

#' @title Distribution Kurtosis
#' @name kurtosis
#' @description Kurtosis of a distribution
#'
#' @usage kurtosis(object, excess = TRUE, ...)
#'
#' @param object Distribution.
#' @param excess logical, if TRUE (default) excess Kurtosis returned.
#' @param ... Passed to `$genExp`.
#'
#' @return Kurtosis as a numeric.
#'
#' @export
NULL

#' @name variance
#' @title Distribution Variance
#' @description The variance or covariance of a distribution, either calculated analytically if
#' or estimated numerically.
#'
#' @usage variance(object, ...)
#'
#' @param object Distribution.
#' @param ... Passed to `$genExp`.
#'
#' @return Variance as a numeric.
#'
#' @export
NULL

#' @title Kth Moment
#' @name kthmoment
#' @description Kth standardised or central moment of a distribution
#'
#' @usage kthmoment(object, k, type = c("central", "standard", "raw"), ...)
#'
#' @param object Distribution.
#' @param k the kth moment to calculate
#' @param type one of 'central', 'standard' or 'raw', abbreviations allowed
#' @param ... Passed to `$genExp`.
#'
#' @return If univariate, the given k-moment as a numeric, otherwise NULL.
#'
#' @export
NULL

#' @title Generalised Expectation of a Distribution
#' @name genExp
#'
#' @usage genExp(object, trafo = NULL, cubature = FALSE, ...)
#'
#' @param object Distribution.
#' @param trafo transformation for expectation calculation, see details.
#' @param cubature If `TRUE` uses [cubature::cubintegrate] for approximation, otherwise [integrate].
#' @param ... Passed to [cubature::cubintegrate].
#'
#' @description A generalised expectation function for distributions, for arithmetic mean and more
#' complex numeric calculations.
#'
#' @return The given expectation as a numeric, otherwise NULL.
#'
#' @export
NULL

#' @title Mode of a Distribution
#' @name mode
#' @description A numeric search for the mode(s) of a distribution.
#'
#' @usage mode(object, which = "all")
#'
#' @param object Distribution.
#' @param which which mode of the distribution should be returned, default is all.
#'
#' @details If the distribution has multiple modes, all are returned by default. Otherwise the index
#' of the mode to return can be given or "all" if all should be returned.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use
#' the \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}.
#'
#' @return The estimated mode as a numeric, either all modes (if multiple) or the ordered mode given
#' in \code{which}.
#'
#' @export
NULL

#' @title Distribution Mean
#'
#' @param x Distribution.
#' @param ... Passed to `$genExp`.
#'
#' @description Arithmetic mean for the probability distribution.
#'
#' @return Mean as a numeric.
#'
#' @export
mean.Distribution <- function(x, ...) {}
