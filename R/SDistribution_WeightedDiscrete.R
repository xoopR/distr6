
#' @name WeightedDiscrete
#' @template SDist
#' @templateVar ClassName WeightedDiscrete
#' @templateVar DistName WeightedDiscrete
#' @templateVar uses in empirical estimators such as Kaplan-Meier
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @details
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the x values and the pdf as the probabilities. The cdf and quantile assume that the
#' elements are supplied in an indexed order (otherwise the results are meaningless).
#'
#' The number of points in the distribution cannot be changed after construction.
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
#' x <- WeightedDiscrete$new(x = 1:3, pdf = c(1 / 5, 3 / 5, 1 / 5))
#' WeightedDiscrete$new(x = 1:3, cdf = c(1 / 5, 4 / 5, 1)) # equivalently
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
WeightedDiscrete <- R6Class("WeightedDiscrete",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "WeightedDiscrete",
    short_name = "WeightDisc",
    description = "Weighted Discrete Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param data `([data.frame])`\cr
    #' Deprecated. Use `x, pdf, cdf`.
    #' @param x `numeric()`\cr
    #' Data samples.
    #' @param pdf `numeric()`\cr
    #' Probability mass function for corresponding samples, should be same length `x`.
    #' If `cdf` is not given then calculated as `cumsum(pdf)`.
    #' @param cdf `numeric()`\cr
    #' Cumulative distribution function for corresponding samples, should be same length `x`. If
    #' given then `pdf` is ignored and calculated as difference of `cdf`s.
    initialize = function(data = NULL, x = 1, pdf = 1,
                          cdf = NULL, decorators = NULL) {

      if (any(duplicated(x))) {
        stop("Values in 'x' must be unique.")
      }

      if (!is.null(data)) {
        warning("'data' constructor now deprecated, use 'x', 'pdf', 'cdf' instead.")
        x <- data$x
        pdf <- data$pdf
        cdf <- data$cdf
      }

      ord <- order(x)
      x <- x[ord]

      if (!is.null(pdf)) {
        pdf <- pdf[ord]
      }
      if (!is.null(cdf)) {
        cdf <- cdf[ord]
      }


      private$.parameters <- getParameterSet(self, x = x, pdf = pdf, cdf = cdf)
      if (!is.null(cdf)) pdf <- rep(1, length(x))
      self$setParameterValue(pdf = pdf, cdf = cdf)

      super$initialize(
        decorators = decorators,
        support = Set$new(x, class = "numeric"),
        type = Reals$new()
      )
    },

    #' @description
    #' Printable string representation of the `Distribution`. Primarily used internally.
    #' @param n `(integer(1))` \cr
    #' Ignored.
    strprint = function(n = 2) {
      "WeightDisc()"
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      x <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (checkmate::testList(x)) {
        return(mapply(function(x0, pdf0) sum(x0 * pdf0), x, pdf))
      } else {
        return(sum(self$getParameterValue("x") * self$getParameterValue("pdf")))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      x <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (checkmate::testList(x)) {
        if (which == "all") {
          stop("`which` cannot be `'all'` when vectorising.")
        } else {
          return(mapply(function(x0, pdf0) {
            pdf0 <- round(pdf0, 10)
            modes <- x0[pdf0 == max(pdf0)]
            if (which > length(modes)) {
              return(modes[length(modes)])
            } else {
              return(modes[which])
            }
          }, x, pdf))
        }
      } else {
        pdf <- round(pdf, 10)
        if (which == "all") {
          return(x[pdf == max(pdf)])
        } else {
          return(x[pdf == max(pdf)][which])
        }
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      x <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")
      mean <- self$mean()

      if (checkmate::testList(x)) {
        return(mapply(function(x0, pdf0, mean0) sum((x0 - mean0)^2 * pdf0), x, pdf, mean))
      } else {
        return(sum((x - mean)^2 * pdf))
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      x <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")
      mean <- self$mean()
      sd <- self$stdev()

      if (checkmate::testList(x)) {
        return(mapply(
          function(x0, pdf0, mean0, sd0) sum(((x0 - mean0) / sd0)^3 * pdf0),
          x, pdf, mean, sd
        ))
      } else {
        return(sum(((x - mean) / sd)^3 * pdf))
      }
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      x <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")
      mean <- self$mean()
      sd <- self$stdev()

      if (checkmate::testList(x)) {
        kurt <- mapply(
          function(x0, pdf0, mean0, sd0) sum(((x0 - mean0) / sd0)^4 * pdf0),
          x, pdf, mean, sd
        )
      } else {
        kurt <- sum(((x - mean) / sd)^4 * pdf)
      }

      if (excess) {
        return(kurt - 3)
      } else {
        return(kurt)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      pdf <- self$getParameterValue("pdf")
      if (checkmate::testList(pdf)) {
        return(sapply(pdf, function(x) -sum(x * log(x, base))))
      } else {
        return(-sum(pdf * log(pdf, base)))
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      data <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (length(t) == 1) {
        return(sum(exp(data * t) * (pdf)))
      } else {
        nr <- length(t)
        nc <- length(data)
        return(as.numeric(
          exp(matrix(data, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      data <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (length(t) == 1) {
        return(sum(exp(data * t * 1i) * (pdf)))
      } else {
        nr <- length(t)
        nc <- length(data)
        return(as.complex(
          exp(matrix(data * 1i, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      data <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (length(z) == 1) {
        return(sum((z^data) * pdf))
      } else {
        nr <- length(z)
        nc <- length(data)
        return(as.numeric(
          (matrix(z, nrow = nr, ncol = nc)^matrix(data, nrow = nr, ncol = nc, byrow = z)) %*%
            matrix(pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$cdf)) lst$pdf <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      data <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (checkmate::testList(data)) {
        # hacky fix for uneven vectors
        lng <- min(sapply(data, length))
        for (i in seq_along(pdf)) {
          pdf[[i]] <- pdf[[i]][seq.int(lng)]
          data[[i]] <- data[[i]][seq.int(lng)]
        }
        pdf <- matrix(unlist(pdf), nrow = length(data[[1]]), ncol = length(data))
        data <- matrix(unlist(data), ncol = ncol(pdf))
        return(C_Vec_WeightedDiscretePdf(x, data, pdf, log))
      } else {
        return(C_WeightedDiscretePdf(x, data, pdf, log))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("x")
      cdf <- self$getParameterValue("cdf")

      if (checkmate::testList(data)) {
        # hacky fix for uneven vectors
        lng <- min(sapply(data, length))
        for (i in seq_along(cdf)) {
          cdf[[i]] <- cdf[[i]][seq.int(lng)]
          data[[i]] <- data[[i]][seq.int(lng)]
        }
        cdf <- matrix(unlist(cdf), nrow = length(data[[1]]), ncol = length(data))
        data <- matrix(unlist(data), ncol = ncol(cdf))
        return(C_Vec_WeightedDiscreteCdf(x, data, cdf, lower.tail, log.p))
      } else {
        return(C_WeightedDiscreteCdf(x, data, cdf, lower.tail, log.p))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("x")
      cdf <- self$getParameterValue("cdf")

      if (checkmate::testList(data)) {
        # hacky fix for uneven vectors
        lng <- min(sapply(data, length))
        for (i in seq_along(cdf)) {
          cdf[[i]] <- cdf[[i]][seq.int(lng)]
          data[[i]] <- data[[i]][seq.int(lng)]
        }
        cdf <- matrix(unlist(cdf), nrow = length(data[[1]]), ncol = length(data))
        data <- matrix(unlist(data), ncol = ncol(cdf))
        return(C_Vec_WeightedDiscreteQuantile(p, data, cdf, lower.tail, log.p))
      } else {
        return(C_WeightedDiscreteQuantile(p, data, cdf, lower.tail, log.p))
      }
    },
    .rand = function(n) {
      data <- self$getParameterValue("x")
      pdf <- self$getParameterValue("pdf")

      if (checkmate::testList(data)) {
        rand <- matrix(nrow = n, ncol = length(pdf))
        for (i in seq_along(data)) {
          rand[, i] <- sample(data[[i]], n, TRUE, pdf[[i]])
        }
      } else {
        rand <- sample(data, n, TRUE, pdf)
      }
      return(rand)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .data = "Deprecated - use self$getParameterValue instead."
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "WeightDisc", ClassName = "WeightedDiscrete",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = ""
  )
)
