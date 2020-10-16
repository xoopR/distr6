# nolint start
#' @name DiscreteUniform
#' @template SDist
#' @templateVar ClassName DiscreteUniform
#' @templateVar DistName Discrete Uniform
#' @templateVar uses as a discrete variant of the more popular Uniform distribution, used to model events with an equal probability of occurring (e.g. role of a die)
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(b - a + 1)}
#' @templateVar paramsupport \eqn{a, b \ \in \ Z; \ b \ge a}{a, b \epsilon Z; b \ge a}
#' @templateVar distsupport \eqn{\{a, a + 1,..., b\}}{{a, a + 1,..., b}}
# nolint end
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
DiscreteUniform <- R6Class("DiscreteUniform",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "DiscreteUniform",
    short_name = "DUnif",
    description = "Discrete Uniform Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param lower `(integer(1))`\cr
    #' Lower limit of the [Distribution], defined on the Naturals.
    #' @param upper `(integer(1))`\cr
    #' Upper limit of the [Distribution], defined on the Naturals.
    initialize = function(lower = 0, upper = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, lower, upper)
      self$setParameterValue(lower = lower, upper = upper)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper, class = "integer"),
        symmetry = "sym",
        type = Integers$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      (unlist(self$getParameterValue("lower")) + unlist(self$getParameterValue("upper"))) / 2
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        if (which == "all") {
          stop("`which` cannot be `'all'` when vectorising.")
        }

        return(mapply(function(x, y) {
          if (which > length(x:y)) {
            return(y)
          } else {
            return((x:y)[which])
          }
        }, lower, upper))
      } else {
        if (which == "all") {
          return(lower:upper)
        } else {
          return((lower:upper)[which])
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
      ((unlist(self$getParameterValue("upper")) -
        unlist(self$getParameterValue("lower")) + 1)^2 - 1) / 12
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      numeric(length(self$getParameterValue("upper")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      upper <- unlist(self$getParameterValue("upper"))
      lower <- unlist(self$getParameterValue("lower"))
      N <- upper - lower + 1
      exkurtosis <- (-6 * (N^2 + 1)) / (5 * (N^2 - 1))
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
      upper <- unlist(self$getParameterValue("upper"))
      lower <- unlist(self$getParameterValue("lower"))
      N <- upper - lower + 1
      return(log(N, base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      upper <- unlist(self$getParameterValue("upper"))
      lower <- unlist(self$getParameterValue("lower"))
      N <- upper - lower + 1

      num <- exp(t * lower) - exp((upper + 1) * t)
      denom <- N * (1 - exp(t))
      return(num / denom)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      upper <- unlist(self$getParameterValue("upper"))
      lower <- unlist(self$getParameterValue("lower"))
      N <- upper - lower + 1

      num <- exp(1i * t * lower) - exp((upper + 1) * t * 1i)
      denom <- N * (1 - exp(1i * t))
      return(num / denom)
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      upper <- unlist(self$getParameterValue("upper"))
      lower <- unlist(self$getParameterValue("lower"))
      N <- upper - lower + 1


      return(1 / N * sum(z^(1:N))) # nolint
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      private$.properties$support <-
        Set$new(self$getParameterValue("lower"):self$getParameterValue("upper"))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::ddunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::ddunif(
          x,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::pdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pdunif(
          x,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::qdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qdunif(
          p,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::rdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rdunif(
          n,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "DUnif", ClassName = "DiscreteUniform",
    Type = "\u2124", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "limits"
  )
)
