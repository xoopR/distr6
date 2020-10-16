# nolint start
#' @name Binomial
#' @template SDist
#' @templateVar ClassName Binomial
#' @templateVar DistName Binomial
#' @templateVar uses to model the number of successes out of a number of independent trials
#' @templateVar params number of trials, n, and probability of success, p,
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(n, x)p^x(1-p)^{n-x}}
#' @templateVar paramsupport \eqn{n = 0,1,2,\ldots} and probability \eqn{p}, where \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{{0, 1,...,n}}
# nolint end
#' @template param_prob
#' @template param_qprob
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template field_packages
#'
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
Binomial <- R6Class("Binomial",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Binomial",
    short_name = "Binom",
    description = "Binomial Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param size `(integer(1))`\cr
    #' Number of trials, defined on the positive Naturals.
    initialize = function(size = 10, prob = 0.5, qprob = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, size, prob, qprob)
      self$setParameterValue(size = size, prob = prob, qprob = qprob)

      super$initialize(
        decorators = decorators,
        support = Set$new(0:size, class = "integer"),
        type = Naturals$new(),
        symmetry = if (prob == 0.5) "symm" else "asym"
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("size")) * unlist(self$getParameterValue("prob"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      sapply((unlist(self$getParameterValue("size")) + 1) *
        unlist(self$getParameterValue("prob")), floor)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      return(unlist(self$getParameterValue("size")) * prob * qprob)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      (1 - (2 * unlist(self$getParameterValue("prob")))) / self$stdev()
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      prob <- unlist(self$getParameterValue("prob"))
      exkurtosis <- (1 - (6 * prob * (1 - prob))) / self$variance()
      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      0.5 * log(2 * pi * exp(1) * self$variance(), base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      (self$getParameterValue("qprob") +
        (self$getParameterValue("prob") * exp(t)))^self$getParameterValue("size")
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      (self$getParameterValue("qprob") +
        (self$getParameterValue("prob") * exp((0 + 1i) * t)))^self$getParameterValue("size")
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      (self$getParameterValue("qprob") +
        (self$getParameterValue("prob") * z))^self$getParameterValue("size")
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$qprob)) lst$prob <- NULL
      super$setParameterValue(lst = lst, error = error)
      private$.properties$support <- Set$new(0:self$getParameterValue("size"))
      if (self$getParameterValue("prob") == 0.5) {
        private$.properties$symmetry <- "asymmetric"
      } else {
        private$.properties$symmetry <- "symmetric"
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "dbinom",
        x = x,
        args = list(
          size = unlist(size),
          prob = unlist(prob)
        ),
        log = log,
        vec = test_list(size)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "pbinom",
        x = x,
        args = list(
          size = unlist(size),
          prob = unlist(prob)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(size)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "qbinom",
        x = p,
        args = list(
          size = unlist(size),
          prob = unlist(prob)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(size)
      )
    },
    .rand = function(n) {
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "rbinom",
        x = n,
        args = list(
          size = unlist(size),
          prob = unlist(prob)
        ),
        vec = test_list(size)
      )
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Binom", ClassName = "Binomial",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats", Tags = "limits"
  )
)
