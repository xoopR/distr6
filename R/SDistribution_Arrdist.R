#' @name Arrdist
#' @template SDist
#' @templateVar ClassName Arrdist
#' @templateVar DistName Arrdist
#' @templateVar uses in matrixed Bayesian estimators such as Kaplan-Meier with confidence bounds over arbitrary dimensions
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_{ijk}) = p_{ijk}}
#' @templateVar paramsupport \eqn{p_{ijk}, i = 1,\ldots,a, j = 1,\ldots,b; \sum_i p_{ijk} = 1}
#' @templateVar distsupport \eqn{x_{111},...,x_{abc}}
#' @templateVar default array(0.5, c(2, 2, 2), list(NULL, 1:2, NULL))
#' @details
#' This is a generalised case of [Matdist] with a third dimension over an arbitrary length.
#' By default all results are returned for the median curve as determined by
#' `(dim(a)[3L] + 1)/2` where `a` is the array and assuming third dimension is odd,
#' this can be changed by setting the `which.curve` parameter.
#'
#' Given the complexity in construction, this distribution is not mutable (cannot be updated after construction).
#'
#' @template class_distribution
#' @template field_alias
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
#' x <- Arrdist$new(pdf = array(0.5, c(3, 2, 4),
#'                  dimnames = list(NULL, 1:2, NULL)))
#' Arrdist$new(cdf = array(c(0.5, 0.5, 0.5, 1, 1, 1), c(3, 2, 4),
#'                         dimnames = list(NULL, 1:2, NULL))) # equivalently
#'
#' # d/p/q/r
#' x$pdf(1)
#' x$cdf(1:2) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#'
#' # Changing which.curve
#' arr <- array(runif(90), c(3, 2, 5), list(NULL, 1:2, NULL))
#' arr <- aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
#' arr[, , 1:3]
#' x <- Arrdist$new(arr)
#' x$mean() # default 0.5 quantile (in this case index 3)
#' x$setParameterValue(which.curve = 3) # equivalently
#' x$mean()
#' # 1% quantile
#' x$setParameterValue(which.curve = 0.01)
#' x$mean()
#' # 5th index
#' x$setParameterValue(which.curve = 5)
#' x$mean()
#' # mean
#' x$setParameterValue(which.curve = "mean")
#' x$mean()
#' @export
Arrdist <- R6Class("Arrdist",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Arrdist",
    short_name = "Arrdist",
    description = "Array Probability Distribution.",
    alias = "AD",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param pdf `numeric()`\cr
    #' Probability mass function for corresponding samples, should be same length `x`.
    #' If `cdf` is not given then calculated as `cumsum(pdf)`.
    #' @param cdf `numeric()`\cr
    #' Cumulative distribution function for corresponding samples, should be same length `x`. If
    #' given then `pdf` calculated as difference of `cdf`s.
    #' @param which.curve `numeric(1) | character(1)` \cr
    #' Which curve (third dimension) should results be displayed for? If
    #' between (0,1) taken as the quantile of the curves otherwise if greater than 1 taken as the curve index, can also be 'mean'. See examples.
    initialize = function(pdf = NULL, cdf = NULL, which.curve = 0.5, decorators = NULL) {
      super$initialize(
        decorators = decorators,
        support = Set$new(1, class = "numeric")^"n",
        type = Reals$new()^"n"
      )
      d = dim(gprm(self, "pdf"))
      private$.ndists <- d[1L]
      private$.ncol <- d[2L]
      private$.ndims <- d[3L]
      invisible(self)
    },

    #' @description
    #' Printable string representation of the `Distribution`. Primarily used internally.
    #' @param n `(integer(1))` \cr
    #' Ignored.
    strprint = function(n = 2) {
      sprintf("Arrdist(%sx%sx%s)", private$.ndists, private$.ncol,
        private$.ndims)
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' If distribution is improper (F(Inf) != 1, then E_X(x) = Inf).
    #' @param ... Unused.
    mean = function(...) {
      "*" %=% gprm(self, c("x", "pdf", "cdf", "which.curve"))

      .set_improper(apply(
        .extCurve(pdf, which.curve), 1, function(.x) sum(.x * x)),
        .extCurve(cdf, which.curve))
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

      "*" %=% gprm(self, c("x", "pdf", "which.curve"))
      x[apply(.extCurve(pdf, which.curve), 1, which.max)]
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' If distribution is improper (F(Inf) != 1, then var_X(x) = Inf).
    #' @param ... Unused.
    variance = function(...) {
      "*" %=% gprm(self, c("x", "pdf", "which.curve"))
      mean <- self$mean()

      vnapply(seq_len(private$.ndists), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum((x - mean[i])^2 * .extRow(.extCurve(pdf, which.curve), i))
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
      "*" %=% gprm(self, c("x", "pdf", "which.curve"))
      mean <- self$mean()
      sd <- self$stdev()

      vnapply(seq_len(private$.ndists), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum(((x - mean[i]) / sd[i])^3 * .extRow(.extCurve(pdf, which.curve), i))
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
      "*" %=% gprm(self, c("x", "pdf", "which.curve"))
      mean <- self$mean()
      sd <- self$stdev()

      kurt <- vnapply(seq_len(private$.ndists), function(i) {
        if (mean[[i]] == Inf) {
          Inf
        } else {
          sum(((x - mean[i]) / sd[i])^4 * .extRow(.extCurve(pdf, which.curve), i))
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
      "*" %=% gprm(self, c("cdf", "pdf", "which.curve"))
      .set_improper(apply(
        .extCurve(pdf, which.curve), 1,
        function(.x) -sum(.x * log(.x, base))
      ), .extCurve(cdf, which.curve))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then mgf_X(x) = Inf).
    #' @param ... Unused.
    mgf = function(t, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x", "which.curve"))

      if (length(t) == 1) {
        mgf <- apply(.extCurve(pdf, which.curve), 1, function(.y) sum(exp(x * t) * .y))
      } else {
        stopifnot(length(z) == private$.ndists)
        mgf <- vnapply(
          seq_len(private$.ndists),
          function(i) sum(exp(x * t[[i]]) * .extRow(.extCurve(pdf, which.curve), i))
        )
      }

      .set_improper(mgf, .extCurve(cdf, which.curve))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then cf_X(x) = Inf).
    #' @param ... Unused.
    cf = function(t, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x", "which.curve"))

      if (length(t) == 1) {
        cf <- apply(.extCurve(pdf, which.curve), 1, function(.y) sum(exp(x * t * 1i) * .y))
      } else {
        stopifnot(length(z) == private$.ndists)
        cf <- vnapply(
          seq_len(private$.ndists),
          function(i) sum(exp(x * t[[i]] * 1i) * .extRow(.extCurve(pdf, which.curve), i))
        )
      }

      .set_improper(cf, .extCurve(cdf, which.curve))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' If distribution is improper (F(Inf) != 1, then pgf_X(x) = Inf).
    #' @param ... Unused.
    pgf = function(z, ...) {
      "*" %=% gprm(self, c("cdf", "pdf", "x", "which.curve"))

      if (length(z) == 1) {
        pgf <- apply(.extCurve(pdf, which.curve), 1, function(.y) sum((z^x) * .y))
      } else {
        stopifnot(length(z) == private$.ndists)
        pgf <- vnapply(
          seq_len(private$.ndists),
          function(i) sum((z[[i]]^x) * .extRow(.extCurve(pdf, which.curve), i))
        )
      }

      .set_improper(pgf, .extCurve(cdf, which.curve))
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
      "pdf, data, wc" %=% gprm(self, c("pdf", "x", "which.curve"))
      mat <- .extCurve(pdf, wc)
      out <- t(C_Vec_WeightedDiscretePdf(
        x, matrix(data, ncol(mat), private$.ndists), t(mat)))
      if (log) {
        out <- log(out)
      }
      colnames(out) <- x
      t(out)
    },

    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) { # FIXME
      "cdf, data, wc" %=% gprm(self, c("cdf", "x", "which.curve"))
      mat <- .extCurve(cdf, wc)
      out <- t(C_Vec_WeightedDiscreteCdf(
        x, matrix(data, ncol(mat), nrow(mat)), t(mat), lower.tail, log.p
      ))
      colnames(out) <- x
      t(out)
    },

    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      "*" %=% gprm(self, c("cdf", "x", "which.curve"))
      mat <- .extCurve(cdf, which.curve)
      out <- t(C_Vec_WeightedDiscreteQuantile(p,
        matrix(x, ncol(mat), nrow(mat)), t(mat), lower.tail, log.p))
      colnames(out) <- NULL
      t(out)
    },

    .rand = function(n) {
      "*" %=% gprm(self, c("pdf", "x", "which.curve"))
      apply(.extCurve(pdf, which.curve), 1,
        function(.y) sample(x, n, TRUE, .y))
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),
    .improper = FALSE,
    .ncol = 0,
    .ndists = 0,
    .ndims = 0
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Arrdist", ClassName = "Arrdist",
    Type = "\u211D^K", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = "", Alias = "AD"
  )
)

.set_improper <- function(val, cdf) {
  which_improper <- cdf[, ncol(cdf), ] < 1
  val[which_improper] <- Inf
  val
}

#' @title Combine Array Distributions into a Arrdist
#' @description Helper function for quickly combining distributions into a [Arrdist].
#' @param ... array distributions to be concatenated.
#' @param decorators If supplied then adds given decorators, otherwise pulls them from underlying distributions.
#' @return [Arrdist]
#' @examples
#' # create three array distributions with different column names
#' arr <- replicate(3, {
#'   pdf <- runif(400)
#'   arr <- array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
#'   arr <- aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
#'   as.Distribution(arr, fun = "pdf")
#' })
#' do.call(c, arr)
#' @export
c.Arrdist <- function(..., decorators = NULL) {
  # get the pdfs and decorators
  pdfdec <- unlist(lapply(list(...), function(x) list(gprm(x, "pdf"), x$decorators)),
    recursive = FALSE
  )
  pdfs <- pdfdec[seq.int(1, length(pdfdec), by = 2)]


  if (is.null(decorators)) {
    decorators <- unique(unlist(pdfdec[seq.int(2, length(pdfdec), by = 2)]))
  }

  nt <- unique(vapply(pdfs, function(.x) dim(.x)[3L], integer(1)))
  if (length(nt) > 1) {
    stop("Can't combine array distributions with different lengths on third dimension.")
  }

  pdfs <- .merge_arrpdf_cols(pdfs)
  pdfs <- do.call(abind::abind, list(what = pdfs, along = 1))

  as.Distribution(pdfs, fun = "pdf", decorators = decs)
}

#' @title Extract one or more Distributions from an Array distribution
#' @description Extract a [WeightedDiscrete] or [Matdist] or [Arrdist] from a [Arrdist].
#' @param ad [Arrdist] from which to extract Distributions.
#' @param i indices specifying distributions (first dimension) to extract, all returned if NULL.
#' @param j indices specifying curves (third dimension) to extract, all returned if NULL.
#' @return If `length(i) == 1` and `length(j) == 1` then returns a [WeightedDiscrete] otherwise if
#' `j` is `NULL` returns an [Arrdist]. If `length(i)` is greater than 1 or `NULL` returns a
#' [Matdist] if `length(j) == 1`.
#' @usage \method{[}{Arrdist}(ad, i = NULL, j = NULL)
#' @examples
#' pdf <- runif(400)
#' arr <- array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
#' arr <- aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
#' darr <- as.Distribution(arr, fun = "pdf")
#' # WeightDisc
#' darr[1, 1]
#' # Matdist
#' darr[1:2, 1]
#' # Arrdist
#' darr[1:3, 1:2]
#' darr[1, 1:2]
#' @export
"[.Arrdist" <- function(ad, i = NULL, j = NULL) {
  if (is.logical(i)) {
    i <- which(i)
  }

  if (is.logical(j)) {
    j <- which(j)
  }

  if (!length(i) && !length(j)) {
    return(ad)
  }

  if (is.character(j) && j != "mean") {
    stop(sprintf("Parameter 'j' should be 'mean' or a number, instead got '%s'", j))
  }

  cdf1 <- .extRow(.extCurve(gprm(ad, "cdf"), j), i)

  if (length(i) == 1 && length(j) == 1) {
    dstr("WeightedDiscrete",
      x = as.numeric(colnames(cdf1)), cdf = cdf1[1, ],
      decorators = ad$decorators
    )
  } else {
    as.Distribution(cdf1, fun = "cdf", decorators = ad$decorators)
  }
}

.extCurve <- function(arr, x) {
  if (!length(x)) {
    return(arr)
  }

  if (length(x) == 1) {
    if (x == "mean") {
      apply(arr, c(1, 2), mean)
    } else if (x < 1) {
      array(apply(arr, c(1, 2), quantile, x), c(nrow(arr), ncol(arr)),
        dimnames(arr)[c(1, 2)])
    } else {
      array(arr[, , x], c(nrow(arr), ncol(arr)), dimnames(arr)[c(1, 2)])
    }
  } else {
    if (all(x < 1 & x > 0)) {
      aperm(array(apply(arr, c(1, 2), quantile, x), c(length(x), nrow(arr), ncol(arr)),
        c(NULL, dimnames(arr)[c(1, 2)])), c(2, 3, 1))
    } else if (all(x)  >= 1) {
      arr[, , x, drop = FALSE]
    } else {
      stop("All curves to extract must be >1 OR all <1 and >0")
    }
  }
}

.extRow <- function(arr, x) {
  if (!length(x)) {
    return(arr)
  }

  if (length(dim(arr)) == 2) {
    arr[x, , drop = FALSE]
  } else {
    arr[x, , , drop = FALSE]
  }
}
