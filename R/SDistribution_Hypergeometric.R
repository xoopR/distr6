# nolint start
#' @name Hypergeometric
#' @template SDist
#' @templateVar ClassName Hypergeometric
#' @templateVar DistName Hypergeometric
#' @templateVar uses to model the number of successes out of a population containing a known number of possible successes, for example the number of red balls from an urn or red, blue and yellow balls
#' @templateVar params population size, \eqn{N}, number of possible successes, \eqn{K}, and number of draws from the distribution, \eqn{n},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(K, x)C(N-K,n-x)/C(N,n)}
#' @templateVar paramsupport \eqn{N = \{0,1,2,\ldots\}}{N = {0,1,2,\ldots}}, \eqn{n, K = \{0,1,2,\ldots,N\}}{n, K = {0,1,2,\ldots,N}} and \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{\{max(0, n + K - N),...,min(n,K)\}}{{max(0, n + K - N),...,min(n,K)}}
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
Hypergeometric <- R6Class("Hypergeometric",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Hypergeometric",
    short_name = "Hyper",
    description = "Hypergeometric Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param size `(integer(1))`\cr
    #' Population size. Defined on positive Naturals.
    #' @param successes `(integer(1))`\cr
    #' Number of population successes. Defined on positive Naturals.
    #' @param failures `(integer(1))`\cr
    #' Number of population failures. `failures = size - successes`. If given then `successes`
    #' is ignored. Defined on positive Naturals.
    #' @param draws `(integer(1))`\cr
    #' Number of draws from the distribution, defined on the positive Naturals.
    initialize = function(size = 50, successes = 5, failures = NULL, draws = 10,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, size, successes, failures, draws)
      self$setParameterValue(size = size, successes = successes, failures = failures, draws = draws)

      support <- Set$new(max(0, draws + successes - size):min(draws, successes), class = "integer")

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
      unlist(self$getParameterValue("draws")) * unlist(self$getParameterValue("successes")) /
        unlist(self$getParameterValue("size"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      draws <- unlist(self$getParameterValue("draws"))
      successes <- unlist(self$getParameterValue("successes"))
      size <- unlist(self$getParameterValue("size"))
      return(sapply(((draws + 1) * (successes + 1)) / (size + 2), floor))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      draws <- unlist(self$getParameterValue("draws"))
      successes <- unlist(self$getParameterValue("successes"))
      size <- unlist(self$getParameterValue("size"))
      return((draws * successes * (size - successes) * (size - draws)) / (size^2 * (size - 1)))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      draws <- unlist(self$getParameterValue("draws"))
      successes <- unlist(self$getParameterValue("successes"))
      size <- unlist(self$getParameterValue("size"))
      return(((size - 2 * successes) * ((size - 1)^0.5) * (size - 2 * draws)) /
        (((draws * successes * (size - successes) * (size - draws))^0.5) * (size - 2)))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      draws <- unlist(self$getParameterValue("draws"))
      successes <- unlist(self$getParameterValue("successes"))
      size <- unlist(self$getParameterValue("size"))

      exkurtosis <- ((size - 1) * (size^2) * ((size * (size + 1)) - 6 * successes *
                                                (size - successes) -
        6 * draws * (size - draws)) + 6 * draws * successes * (size - successes) *
        (size - draws) * (5 * size - 6)) / (draws * successes * (size - successes) *
                                              (size - draws) * (size - 2) * (size - 3))

      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$failures)) lst$successes <- NULL
      super$setParameterValue(lst = lst, error = error)
      size <- self$getParameterValue("size")

      private$.properties$support <- Set$new(max(0, self$getParameterValue("draws") +
        self$getParameterValue("successes") - size):min(
        self$getParameterValue("draws"),
        self$getParameterValue("successes")
      ))

      pparams <- self$parameters()$.__enclos_env__$private
      pparams$.setParameterSupport(list(successes = Set$new(0:size)))
      pparams$.setParameterSupport(list(draws = Set$new(0:size)))
      pparams$.setParameterSupport(list(failures = Set$new(0:size)))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      m <- self$getParameterValue("successes")
      n <- self$getParameterValue("failures")
      k <- self$getParameterValue("draws")

      call_C_base_pdqr(
        fun = "dhyper",
        x = x,
        args = list(
          m = unlist(m),
          n = unlist(n),
          k = unlist(k)
        ),
        log = log,
        vec = test_list(m)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      m <- self$getParameterValue("successes")
      n <- self$getParameterValue("failures")
      k <- self$getParameterValue("draws")

      call_C_base_pdqr(
        fun = "phyper",
        x = x,
        args = list(
          m = unlist(m),
          n = unlist(n),
          k = unlist(k)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(m)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      m <- self$getParameterValue("successes")
      n <- self$getParameterValue("failures")
      k <- self$getParameterValue("draws")

      call_C_base_pdqr(
        fun = "qhyper",
        x = p,
        args = list(
          m = unlist(m),
          n = unlist(n),
          k = unlist(k)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(m)
      )
    },
    .rand = function(n) {
      nn <- n
      m <- self$getParameterValue("successes")
      n <- self$getParameterValue("failures")
      k <- self$getParameterValue("draws")

      call_C_base_pdqr(
        fun = "rhyper",
        x = nn,
        args = list(
          m = unlist(m),
          n = unlist(n),
          k = unlist(k)
        ),
        vec = test_list(m)
      )
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Hyper", ClassName = "Hypergeometric",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats", Tags = "limits"
  )
)
