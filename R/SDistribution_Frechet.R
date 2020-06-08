
#' @name Frechet
#' @template SDist
#' @templateVar ClassName Frechet
#' @templateVar DistName Frechet
#' @templateVar uses as a special case of the Generalised Extreme Value distribution
#' @templateVar params shape, \eqn{\alpha}, scale, \eqn{\beta}, and minimum, \eqn{\gamma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)((x-\gamma)/\beta)^{-1-\alpha}exp(-(x-\gamma)/\beta)^{-\alpha}}
#' @templateVar paramsupport \eqn{\alpha, \beta \epsilon R^+} and \eqn{\gamma \epsilon R}
#' @templateVar distsupport \eqn{x > \gamma}
#' @templateVar aka Inverse Weibull
#' @aliases InverseWeibull
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_shape
#' @template param_scale
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Frechet <- R6Class("Frechet",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Frechet",
    short_name = "Frec",
    description = "Frechet Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param minimum `(numeric(1))`\cr
    #' Minimum of the distribution, defined on the Reals.
    initialize = function(shape = 1, scale = 1, minimum = 0,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, shape, scale, minimum)
      self$setParameterValue(shape = shape, scale = scale, minimum = minimum)

      super$initialize(
        decorators = decorators,
        support = Interval$new(minimum, Inf, type = "()"),
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      if (self$getParameterValue("shape") <= 1) {
        return(Inf)
      } else {
        return(self$getParameterValue("minimum") + self$getParameterValue("scale") * gamma(1 - 1 / self$getParameterValue("shape")))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      return(self$getParameterValue("minimum") +
        self$getParameterValue("scale") *
          (self$getParameterValue("shape") /
            (1 + self$getParameterValue("shape")))^(1 / self$getParameterValue("shape")))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      m <- self$getParameterValue("minimum")
      s <- self$getParameterValue("scale")
      a <- self$getParameterValue("shape")

      return(m + s / (log(2)^(1 / a)))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      if (self$getParameterValue("shape") <= 2) {
        return(Inf)
      } else {
        return(self$getParameterValue("scale")^2 * (gamma(1 - 2 / self$getParameterValue("shape")) -
          gamma(1 - 1 / self$getParameterValue("shape"))^2))
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      if (self$getParameterValue("shape") <= 3) {
        return(Inf)
      } else {
        shape <- self$getParameterValue("shape")
        num <- gamma(1 - 3 / shape) - 3 * gamma(1 - 2 / shape) * gamma(1 - 1 / shape) + 2 * gamma(1 - 1 / shape)^3
        den <- (gamma(1 - 2 / shape) - gamma(1 - 1 / shape)^2)^(3 / 2)
        return(num / den)
      }
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      if (self$getParameterValue("shape") <= 4) {
        return(Inf)
      } else {
        shape <- self$getParameterValue("shape")
        num <- gamma(1 - 4 / shape) - 4 * gamma(1 - 3 / shape) * gamma(1 - 1 / shape) + 3 * gamma(1 - 2 / shape)^2
        den <- (gamma(1 - 2 / shape) - gamma(1 - 1 / shape)^2)^2
        if (excess) {
          return(-6 + num / den)
        } else {
          return(-3 + num / den)
        }
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      return(1 - digamma(1) / self$getParameterValue("shape") - digamma(1) +
        log(self$getParameterValue("scale") / self$getParameterValue("shape"), base))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      private$.properties$support <- Interval$new(self$getParameterValue("minimum"), Inf, type = "()")
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::dfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dfrechet(
          x,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::pfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pfrechet(
          x,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qfrechet(
          p,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rfrechet,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rfrechet(
          n,
          lambda = self$getParameterValue("shape"),
          mu = self$getParameterValue("minimum"),
          sigma = self$getParameterValue("scale")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$minimum)) lst <- c(lst, list(minimum = paramlst$minimum))
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Frec", ClassName = "Frechet",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
