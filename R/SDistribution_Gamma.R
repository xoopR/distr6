#' @name Gamma
#' @template SDist
#' @templateVar ClassName Gamma
#' @templateVar DistName Gamma
#' @templateVar uses as the prior in Bayesian modelling, the convolution of exponential distributions, and to model waiting times
#' @templateVar params shape, \eqn{\alpha}, and rate, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha)x^{\alpha-1}exp(-x\beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor shape = 1, rate = 1, scale = NULL, mean = NULL
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{rate} \tab numeric \tab inverse scale parameter. \cr
#' @templateVar arg3 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg4 \code{mean} \tab numeric \tab alternate scale parameter. \cr
#' @templateVar constructorDets \code{shape} and either \code{rate}, \code{scale} or \code{mean}, all as positive numerics. These are related via, \deqn{scale = 1/rate} \deqn{mean = shape/rate} If \code{mean} is given then \code{rate} and \code{scale} are ignored. If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Gamma$new(shape = 1, rate = 2)
#' Gamma$new(shape = 1, scale = 4)
#' Gamma$new(shape = 1, mean = 0.5)
#'
#' # Default is shape = 1, rate = 1
#' x <- Gamma$new(verbose = TRUE)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(scale = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
NULL

Gamma <- R6Class("Gamma", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Gamma",
    short_name = "Gamma",
    description = "Gamma Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, rate = 1, scale = NULL, mean = NULL, decorators = NULL,
                          verbose = FALSE) {

      private$.parameters <- getParameterSet.Gamma(self, shape, rate, scale, mean)
      self$setParameterValue(shape = shape, rate = rate, scale = scale, mean = mean)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = F),
        type = PosReals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = 'all') {
      if (self$getParameterValue("shape") >= 1) {
        return((self$getParameterValue("shape") - 1) / self$getParameterValue("rate"))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(self$getParameterValue("mean") * self$getParameterValue("scale"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      2 / sqrt(self$getParameterValue("shape"))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(6 / self$getParameterValue("shape"))
      } else {
        return((6 / self$getParameterValue("shape")) + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      self$getParameterValue("shape") - log(self$getParameterValue("rate"), base) +
        log(gamma(self$getParameterValue("shape")), base) + (1 - self$getParameterValue("shape")) * digamma(self$getParameterValue("shape"))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      if (t < self$getParameterValue("rate")) {
        return((1 - self$getParameterValue("scale") * t)^(-self$getParameterValue("shape")))
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      return((1 - self$getParameterValue("scale") * 1i * t)^(-self$getParameterValue("shape")))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        log = log,
        vec = test_list(shape)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "pgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qgamma",
        x = p,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .rand = function(n) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rgamma",
        x = n,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        vec = test_list(shape)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
      if (!is.null(paramlst$rate)) lst <- c(lst, list(rate = paramlst$rate))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(rate = paramlst$scale^-1))
      if (!is.null(paramlst$mean)) {
        if (is.null(paramlst$shape)) {
          lst <- c(lst, list(rate = self$getParameterValue("shape") / paramlst$mean))
        } else {
          lst <- c(lst, list(rate = paramlst$shape / paramlst$mean))
        }
      }

      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Gamma", ClassName = "Gamma",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
