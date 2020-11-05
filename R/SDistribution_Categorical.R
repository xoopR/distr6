# nolint start
#' @name Categorical
#' @template SDist
#' @templateVar ClassName Categorical
#' @templateVar DistName Categorical
#' @templateVar uses in classification supervised learning
#' @templateVar params a given support set, \eqn{x_1,...,x_k}, and respective probabilities, \eqn{p_1,...,p_k},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @details
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the support set and the probabilities from the `probs` parameter. The cdf and quantile assumes
#' that the elements are supplied in an indexed order (otherwise the results are meaningless).
#'
#' The number of points in the distribution cannot be changed after construction.
#'
# nolint end
#
#
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
#' @export
Categorical <- R6Class("Categorical",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Categorical",
    short_name = "Cat",
    description = "Categorical Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param elements `list()`\cr
    #' Categories in the distribution, see examples.
    #' @param probs `numeric()`\cr
    #' Probabilities of respective categories occurring.
    #'
    #' @examples
    #' # Note probabilities are automatically normalised (if not vectorised)
    #' x <- Categorical$new(elements = list("Bapple", "Banana", 2), probs = c(0.2, 0.4, 1))
    #'
    #' # Length of elements and probabilities cannot be changed after construction
    #' x$setParameterValue(probs = c(0.1, 0.2, 0.7))
    #'
    #' # d/p/q/r
    #' x$pdf(c("Bapple", "Carrot", 1, 2))
    #' x$cdf("Banana") # Assumes ordered in construction
    #' x$quantile(0.42) # Assumes ordered in construction
    #' x$rand(10)
    #'
    #' # Statistics
    #' x$mode()
    #'
    #' summary(x)
    initialize = function(elements = 1, probs = 1, decorators = NULL) {

      checkmate::assert(length(elements) == length(probs))

      private$.parameters <- getParameterSet(self, probs, elements)
      self$setParameterValue(probs = probs, elements = elements)

      super$initialize(
        decorators = decorators,
        support = Set$new(lst = elements),
        type = Universal$new(),
        symmetry = if (length(unique(self$getParameterValue("probs"))) == 1) "sym" else "asym"
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      p <- self$getParameterValue("probs")
      if (checkmate::testList(p)) {
        return(rep(NaN, length(p)))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      probs <- self$getParameterValue("probs")
      els <- self$getParameterValue("elements")
      if (!checkmate::testList(probs)) {
        modes <- unlist(els[probs == max(probs)])
        if (which == "all") {
          return(modes)
        } else {
          return(modes[which])
        }
      } else {
        if (which == "all") {
          stop("`which` cannot be `'all'` when vectorising.")
        } else {
          modes <- c()
          for (i in seq_along(probs)) {
            m <- (els[[i]][probs[[i]] == max(probs[[i]])])
            if (which > length(m)) {
              m <- m[length(m)]
            } else {
              m <- m[which]
            }
            modes <- c(modes, m)
          }
          return(modes)
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
      p <- self$getParameterValue("probs")
      if (checkmate::testList(p)) {
        return(rep(NaN, length(p)))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      p <- self$getParameterValue("probs")
      if (checkmate::testList(p)) {
        return(rep(NaN, length(p)))
      } else {
        return(NaN)
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
      p <- self$getParameterValue("probs")
      if (checkmate::testList(p)) {
        return(rep(NaN, length(p)))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      p <- self$getParameterValue("probs")
      if (checkmate::testList(p)) {
        return(rep(NaN, length(p)))
      } else {
        return(NaN)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(NaN)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(NaN)
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {

      super$setParameterValue(..., lst = lst, error = error)

      if (length(unique(self$getParameterValue("probs"))) == 1) {
        private$.properties$symmetry <- "asymmetric"
      } else {
        private$.properties$symmetry <- "symmetric"
      }

      private$.properties$support <- Set$new(elements = self$getParameterValue("elements"))

      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      probs <- self$getParameterValue("probs")
      els <- self$getParameterValue("elements")

      if (checkmate::testList(probs)) {
        probs <- matrix(unlist(probs), nrow = length(probs[[1]]), ncol = length(probs))
        els <- matrix(unlist(els), ncol = ncol(probs))
        pdf <- matrix(nrow = length(x), ncol = ncol(probs))
        for (i in seq(ncol(probs))) {
          els_ind <- seq_along(els[, i])
          new_x <- match(x, els[, i])
          pdf[, i] <- C_WeightedDiscretePdf(new_x, els_ind, probs[, i], log)
        }
        return(pdf)
      } else {
        els_ind <- seq_along(els)
        new_x <- match(x, els)
        return(C_WeightedDiscretePdf(new_x, els_ind, probs, log))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {

      probs <- self$getParameterValue("probs")
      els <- self$getParameterValue("elements")

      if (checkmate::testList(probs)) {
        probs <- matrix(unlist(probs), nrow = length(probs[[1]]), ncol = length(probs))
        els <- matrix(unlist(els), ncol = ncol(probs))
        cdf <- matrix(nrow = length(x), ncol = ncol(probs))
        for (i in seq(ncol(probs))) {
          els_ind <- seq_along(els[, i])
          new_x <- match(x, els[, i])
          new_cdf <- cumsum(probs[, i])
          cdf[, i] <- C_WeightedDiscreteCdf(new_x, els_ind, new_cdf, lower.tail, log.p)
        }
        return(cdf)
      } else {
        els_ind <- seq_along(els)
        new_x <- match(x, els)
        cdf <- cumsum(probs)
        return(C_WeightedDiscreteCdf(new_x, els_ind, cdf, lower.tail, log.p))
      }

    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {

      probs <- self$getParameterValue("probs")
      els <- self$getParameterValue("elements")

      if (checkmate::testList(probs)) {
        probs <- matrix(unlist(probs), nrow = length(probs[[1]]), ncol = length(probs))
        new_els <- matrix(unlist(els), ncol = ncol(probs))
        quantile <- matrix(nrow = length(p), ncol = ncol(probs))
        for (i in seq(ncol(probs))) {
          els_ind <- seq_along(new_els[, i])
          # new_x <- match(x, els[, i])
          new_cdf <- cumsum(probs[, i])
          quantile[, i] <- C_WeightedDiscreteQuantile(p, els_ind, new_cdf, lower.tail, log.p)
          quantile[, i] <- unlist(els[[i]][quantile[, i]])
        }
        return(quantile)
      } else {
        els_ind <- seq_along(els)
        cdf <- cumsum(probs)
        quantile <- C_WeightedDiscreteQuantile(p, els_ind, cdf, lower.tail, log.p)
        return(unlist(els[quantile]))
      }
    },
    .rand = function(n) {
      els <- self$getParameterValue("elements")
      probs <- self$getParameterValue("probs")

      if (checkmate::testList(probs)) {
        rand <- matrix(nrow = n, ncol = length(probs))
        for (i in seq_along(probs)) {
          rand[, i] <- unlist(sample(els[[i]], n, TRUE, probs[[i]]))
        }
      } else {
        rand <- sample(els, n, TRUE, probs)
      }
      return(rand)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Cat", ClassName = "Categorical",
    Type = "V", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = ""
  )
)
