#' @name Matdist
#' @template SDist
#' @templateVar ClassName Matdist
#' @templateVar DistName Matdist
#' @templateVar uses in vectorised empirical estimators such as Kaplan-Meier
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_{ij}) = p_{ij}}
#' @templateVar paramsupport \eqn{p_{ij}, i = 1,\ldots,k, j = 1,\ldots,n; \sum_i p_{ij} = 1}
#' @templateVar distsupport \eqn{x_{11},...,x_{kn}}
#' @templateVar default matrix(0.5, 2, 2, dimnames = list(NULL, 1:2))
#' @details
#' This is a special case distribution in distr6 which is technically a vectorised distribution
#' but is treated as if it is not. Therefore we only allow evaluation of all functions at
#' the same value, e.g. `$pdf(1:2)` evaluates all samples at '1' and '2'.
#'
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the x values and the pdf as the probabilities. The cdf and quantile assume that the
#' elements are supplied in an indexed order (otherwise the results are meaningless).
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#'
#' @family discrete distributions
#' @family univariate distributions
#'
#' @examples
#' x <- Matdist$new(pdf = matrix(0.5, 2, 2, dimnames = list(NULL, 1:2)))
#' Matdist$new(cdf = matrix(c(0.5, 1), 2, 2, TRUE, dimnames = list(NULL, c(1, 2)))) # equivalently
#'
#' # d/p/q/r
#' x$pdf(1:5)
#' x$cdf(1:5) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
Matdist <- R6Class("Matdist",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Matdist",
    short_name = "Matdist",
    description = "Matrix Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param x `numeric()`\cr
    #' Data samples, *must be ordered in ascending order*.
    #' @param pdf `numeric()`\cr
    #' Probability mass function for corresponding samples, should be same length `x`.
    #' If `cdf` is not given then calculated as `cumsum(pdf)`.
    #' @param cdf `numeric()`\cr
    #' Cumulative distribution function for corresponding samples, should be same length `x`. If
    #' given then `pdf` calculated as difference of `cdf`s.
    initialize = function(pdf = NULL, cdf = NULL, decorators = NULL) {
      super$initialize(
        decorators = decorators,
        support = Set$new(1, class = "numeric")^"n",
        type = Reals$new()^"n"
      )
      invisible(self)
    },

    #' @description
    #' Printable string representation of the `Distribution`. Primarily used internally.
    #' @param n `(integer(1))` \cr
    #' Ignored.
    strprint = function(n = 2) {
      "Matdist()"
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' If distribution is improper (F(Inf) != 1, then E_X(x) = Inf).
    #' @param ... Unused.
    mean = function(...) {
      "*" %=% gprm(self, c("x", "pdf", "cdf"))
      .set_improper(apply(pdf, 1, function(.x) sum(.x * x)), cdf)
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      as.numeric(self$quantile(0.5))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = 1) {
      if (!is.null(which) && which == "all") {
        stop("`which` cannot be `'all'` when vectorising.")
      }

      "*" %=% gprm(self, c("x", "pdf"))
      x[apply(pdf, 1, which.max)]
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' If distribution is improper (F(Inf) != 1, then var_X(x) = Inf).
    #' @param ... Unused.
    variance = function(...) {
      "*" %=% gprm(self, c("x", "pdf"))
      mean <- self$mean()

      vnapply(seq(nrow(pdf)), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum((x - mean[i])^2 * pdf[i, ])
        }
      })
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' If distribution is improper (F(Inf) != 1, then sk_X(x) = Inf).
    #' @param ... Unused.
    skewness = function(...) {
      "*" %=% gprm(self, c("x", "pdf"))
      mean <- self$mean()
      sd <- self$stdev()

      vnapply(seq(nrow(pdf)), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum(((x - mean[i]) / sd[i])^3 * pdf[i, ])
        }
      })
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' If distribution is improper (F(Inf) != 1, then k_X(x) = Inf).
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      "*" %=% gprm(self, c("x", "pdf"))
      mean <- self$mean()
      sd <- self$stdev()

      kurt <- vnapply(seq(nrow(pdf)), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum(((x - mean[i]) / sd[i])^4 * pdf[i, ])
        }
      })

      if (excess) {
        kurt - 3
      } else {
        kurt
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' If distribution is improper then entropy is Inf.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      "*" %=% gprm(self, c("cdf", "pdf"))
      .set_improper(apply(pdf, 1, function(.x) -sum(.x * log(.x, base))), cdf)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then mgf_X(x) = Inf).
    #' @param ... Unused.
    mgf = function(t, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x"))

      if (length(t) == 1) {
        mgf <- apply(pdf, 1, function(.y) sum(exp(x * t) * .y))
      } else {
        stopifnot(length(z) == nrow(pdf))
        mgf <- vnapply(seq(nrow(pdf)),
                        function(i) sum(exp(x * t[[i]]) * pdf[i, ]))
      }

      .set_improper(mgf, cdf)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then cf_X(x) = Inf).
    #' @param ... Unused.
    cf = function(t, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x"))

      if (length(t) == 1) {
        cf <- apply(pdf, 1, function(.y) sum(exp(x * t * 1i) * .y))
      } else {
        stopifnot(length(z) == nrow(pdf))
        cf <- vnapply(seq(nrow(pdf)),
                        function(i) sum(exp(x * t[[i]] * 1i) * pdf[i, ]))
      }

      .set_improper(cf, cdf)
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then pgf_X(x) = Inf).
    #' @param ... Unused.
    pgf = function(z, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x"))

      if (length(z) == 1) {
        pgf <- apply(pdf, 1, function(.y) sum((z^x) * .y))
      } else {
        stopifnot(length(z) == nrow(pdf))
        pgf <- vnapply(seq(nrow(pdf)),
                        function(i) sum((z[[i]]^x) * pdf[i, ]))
      }

      .set_improper(pgf, cdf)
    }
  ),

  active = list(
    #' @field properties
    #' Returns distribution properties, including skewness type and symmetry.
    properties = function() {
      prop <- super$properties
      prop$support <- Set$new(gprm(self, "x"), class = "numeric")
      prop
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      "pdf, data" %=% gprm(self, c("pdf", "x"))
      out <- t(C_Vec_WeightedDiscretePdf(
        x, matrix(data, ncol(pdf), nrow(pdf)),
        t(pdf)
      ))
      if (log) {
        out <- log(out)
      }
      colnames(out) <- x
      out
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) { # FIXME
      "cdf, data" %=% gprm(self, c("cdf", "x"))
      out <- t(C_Vec_WeightedDiscreteCdf(
        x, matrix(data, ncol(cdf), nrow(cdf)),
        t(cdf), lower.tail, log.p
      ))
      colnames(out) <- x
      out
    },

    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      "*" %=% gprm(self, c("cdf", "x"))
      out <- t(C_Vec_WeightedDiscreteQuantile(p, matrix(x, ncol(cdf), nrow(cdf)),
          t(cdf), lower.tail, log.p))
      colnames(out) <- NULL
      out
    },

    .rand = function(n) {
      "*" %=% gprm(self, c("pdf", "x"))
      t(apply(pdf, 1, function(.y) sample(x, n, TRUE, .y)))
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .data = "Deprecated - use self$getParameterValue instead.",
    .improper = FALSE
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Matdist", ClassName = "Matdist",
    Type = "\u211D^K", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = ""
  )
)

.set_improper <- function(val, cdf) {
  which_improper <- cdf[, ncol(cdf)] < 1
  val[which_improper] <- Inf
  val
}