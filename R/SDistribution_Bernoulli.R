# nolint start
#' @name Bernoulli
#' @template SDist
#'
#' @templateVar ClassName Bernoulli
#' @templateVar DistName Bernoulli
#' @templateVar uses to model a two-outcome scenario
#' @templateVar params probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = p, \ if \ x = 1}{f(x) = p, if x = 1}\deqn{f(x) = 1 - p, \ if \ x = 0}{f(x) = 1 - p, if x = 0}
#' @templateVar paramsupport probability \eqn{p}
#' @templateVar distsupport \eqn{\{0,1\}}{{0,1}}
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
Bernoulli <- R6Class("Bernoulli",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Bernoulli",
    short_name = "Bern",
    description = "Bernoulli Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(prob = 0.5, qprob = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, prob, qprob)
      self$setParameterValue(prob = prob, qprob = qprob)

      super$initialize(
        decorators = decorators,
        support = Set$new(0, 1, class = "integer"),
        type = Naturals$new(),
        symmetry = if (self$getParameterValue("prob") == 0.5) "symmetric" else "asymmetric"
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("prob"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      prob <- unlist(self$getParameterValue("prob"))

      if (length(prob) > 1) {
        if (which == "all") {
          stop("`which` cannot be `'all'` when vectorising.")
        } else {
          mode <- numeric(length(prob))
          mode[prob < 0.5] <- 0
          mode[prob > 0.5] <- 1
          mode[prob == 0.5] <- c(0, 1)[which]
        }
      } else {
        if (prob < 0.5) {
          mode <- 0
        } else if (prob > 0.5) {
          mode <- 1
        } else {
          if (which == "all") {
            mode <- c(0, 1)
          } else {
            mode <- c(0, 1)[which]
          }
        }
      }

      return(mode)
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      prob <- self$getParameterValue("prob")
      median <- numeric(length(prob))
      median[prob < 0.5] <- 0
      median[prob > 0.5] <- 1
      median[prob == 0.5] <- NaN

      return(median)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      unlist(self$getParameterValue("prob")) * unlist(self$getParameterValue("qprob"))
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
      exkurtosis <- (1 - (6 * unlist(self$getParameterValue("prob")) *
        unlist(self$getParameterValue("qprob")))) / self$variance()
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
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      return((-qprob * log(qprob, base)) + (-prob * log(prob, base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob

      qprob + (prob * exp(t))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob

      qprob + (prob * exp(1i * t))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob
      return(qprob + (prob * z))
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$qprob)) lst$prob <- NULL
      super$setParameterValue(lst = lst, error = error)
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
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "dbinom",
        x = x,
        args = list(
          size = 1,
          prob = unlist(prob)
        ),
        log = log,
        vec = test_list(prob)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "pbinom",
        x = x,
        args = list(
          size = 1,
          prob = unlist(prob)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "qbinom",
        x = p,
        args = list(
          size = 1,
          prob = unlist(prob)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )
    },
    .rand = function(n) {
      prob <- self$getParameterValue("prob")

      call_C_base_pdqr(
        fun = "rbinom",
        x = n,
        args = list(
          size = 1,
          prob = unlist(prob)
        ),
        vec = test_list(prob)
      )
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Bern", ClassName = "Bernoulli",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
