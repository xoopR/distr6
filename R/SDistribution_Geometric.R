# nolint start
#' @name Geometric
#' @template SDist
#' @templateVar ClassName Geometric
#' @templateVar DistName Geometric
#' @templateVar uses to model the number of trials (or number of failures) before the first success
#' @templateVar params probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (1 - p)^{k-1}p}
#' @templateVar paramsupport probability \eqn{p}
#' @templateVar distsupport the Naturals (zero is included if modelling number of failures before success)
#' @details
#' The Geometric distribution is used to either refer to modelling the number of trials or number
#' of failures before the first success.
# nolint end
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_prob
#' @template param_qprob
#' @template field_packages
#'
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
Geometric <- R6Class("Geometric",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Geometric",
    short_name = "Geom",
    description = "Geometric Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param trials `(logical(1))` \cr
    #' If `TRUE` then the distribution models the number of trials, \eqn{x}, before the first
    #' success. Otherwise the distribution calculates the probability of \eqn{y} failures before the
    #' first success. Mathematically these are related by \eqn{Y = X - 1}.
    initialize = function(prob = 0.5, qprob = NULL, trials = FALSE, decorators = NULL) {

      private$.parameters <- getParameterSet(self, prob = prob, qprob = qprob, trials = trials)
      self$setParameterValue(prob = prob, qprob = qprob)

      if (!trials) {
        support <- Naturals$new()
        self$description <- "Geometric (Failures) Probability Distribution."
      } else {
        support <- PosNaturals$new()
        self$description <- "Geometric (Trials) Probability Distribution."
      }

      super$initialize(
        decorators = decorators,
        support = support,
        type = Naturals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      if (self$getParameterValue("trials")[[1]]) {
        return(1 / unlist(self$getParameterValue("prob")))
      } else {
        return((1 - unlist(self$getParameterValue("prob"))) /
          unlist(self$getParameterValue("prob")))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (self$getParameterValue("trials")[[1]]) {
        return(numeric(length(self$getParameterValue("prob"))) + 1)
      } else {
        return(numeric(length(self$getParameterValue("prob"))))
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      prob <- unlist(self$getParameterValue("prob"))
      return((1 - prob) / (prob^2))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      prob <- unlist(self$getParameterValue("prob"))
      return((2 - prob) / sqrt(1 - prob))
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
      exkurtosis <- 6 + (prob^2 / (1 - prob))
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
      return(((-(1 - prob) * log(1 - prob, base)) - (prob * log(prob, base))) / prob) # nolint
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      if (self$getParameterValue("trials")[[1]]) {
        if (t < -log(1 - self$getParameterValue("prob"))) {
          return((self$getParameterValue("prob") * exp(t)) /
                   (1 - (1 - self$getParameterValue("prob")) * exp(t)))
        } else {
          return(NaN)
        }
      } else {
        return((self$getParameterValue("prob")) /
                 (1 - (1 - self$getParameterValue("prob")) * exp(t)))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      if (self$getParameterValue("trials")[[1]]) {
        return((self$getParameterValue("prob") * exp(1i * t)) /
                 (1 - (1 - self$getParameterValue("prob")) * exp(1i * t)))
      } else {
        return((self$getParameterValue("prob")) /
                 (1 - (1 - self$getParameterValue("prob")) * exp(1i * t)))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      if (self$getParameterValue("trials")[[1]]) {
        return((self$getParameterValue("prob") * z) / (1 - z * self$getParameterValue("qprob")))
      } else {
        return(self$getParameterValue("prob") / (1 - z * self$getParameterValue("qprob")))
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$qrob)) lst$prob <- NULL
      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (self$getParameterValue("trials")[[1]]) {
        x <- x + 1
      }

      prob <- self$getParameterValue("prob")
      call_C_base_pdqr(
        fun = "dgeom",
        x = x,
        args = list(prob = unlist(prob)),
        log = log,
        vec = test_list(prob)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (self$getParameterValue("trials")[[1]]) {
        x <- x + 1
      }

      prob <- self$getParameterValue("prob")
      call_C_base_pdqr(
        fun = "pgeom",
        x = x,
        args = list(prob = unlist(prob)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      prob <- self$getParameterValue("prob")
      geom <- call_C_base_pdqr(
        fun = "qgeom",
        x = p,
        args = list(prob = unlist(prob)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )

      if (self$getParameterValue("trials")[[1]]) {
        geom <- geom + 1
      }

      return(geom)
    },
    .rand = function(n) {
      prob <- self$getParameterValue("prob")
      geom <- call_C_base_pdqr(
        fun = "rgeom",
        x = n,
        args = list(prob = unlist(prob)),
        vec = test_list(prob)
      )

      if (self$getParameterValue("trials")[[1]]) {
        geom <- geom + 1
      }

      return(geom)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .trials = logical(0)
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Geom", ClassName = "Geometric",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
