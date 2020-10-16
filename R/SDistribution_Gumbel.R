# nolint start
#' @name Gumbel
#' @template SDist
#' @templateVar ClassName Gumbel
#' @templateVar DistName Gumbel
#' @templateVar uses to model the maximum (or minimum) of a number of samples of different distributions, and is a special case of the Generalised Extreme Value distribution
#' @templateVar params location, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(z + exp(-z)))/\beta}
#' @templateVar paramsupport \eqn{z = (x-\mu)/\beta}, \eqn{\mu \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_locationscale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Gumbel <- R6Class("Gumbel",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Gumbel",
    short_name = "Gumb",
    description = "Gumbel Probability Distribution.",
    packages = c("extraDistr", "pracma"),

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(location = 0, scale = 1,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, location, scale)
      self$setParameterValue(location = location, scale = scale)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("location")) -
        digamma(1) * unlist(self$getParameterValue("scale"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("location"))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      unlist(self$getParameterValue("location")) -
        unlist(self$getParameterValue("scale")) * log(log(2))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      (pi * unlist(self$getParameterValue("scale")))^2 / 6
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #'
    #' Apery's Constant to 16 significant figures is used in the calculation.
    #' @param ... Unused.
    skewness = function(...) {
      rep(
        (12 * sqrt(6) * 1.202056903159594285399738161511449990764986292) / pi^3,
        length(self$getParameterValue("scale"))
      )
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      if (excess) {
        return(rep(2.4, length(self$getParameterValue("scale"))))
      } else {
        return(rep(5.4, length(self$getParameterValue("scale"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      log(unlist(self$getParameterValue("scale")), base) - digamma(1) + 1
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      return(gamma(1 - self$getParameterValue("scale") * t) *
               exp(self$getParameterValue("location") * t))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #'
    #' [pracma::gammaz()] is used in this function to allow complex inputs.
    #' @param ... Unused.
    cf = function(t, ...) {
      return(pracma::gammaz(1 - self$getParameterValue("scale") * t * 1i) *
               exp(1i * self$getParameterValue("location") * t))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("location"))) {
        mapply(
          extraDistr::dgumbel,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dgumbel(
          x,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("location"))) {
        mapply(
          extraDistr::pgumbel,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pgumbel(
          x,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("location"))) {
        mapply(
          extraDistr::qgumbel,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qgumbel(
          p,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("location"))) {
        mapply(
          extraDistr::rgumbel,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rgumbel(
          n,
          mu = self$getParameterValue("location"),
          sigma = self$getParameterValue("scale")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Gumb", ClassName = "Gumbel",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "locscale"
  )
)
