
#' @name Normal
#' @template SDist
#' @templateVar ClassName Normal
#' @templateVar DistName Normal
#' @templateVar uses in significance testing, for representing models with a bell curve, and as a result of the central limit theorem
#' @templateVar params variance, \eqn{\sigma^2}, and mean, \eqn{\mu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(x-\mu)^2/(2\sigma^2)) / \sqrt{2\pi\sigma^2}}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\sigma^2 > 0}
#' @templateVar distsupport the Reals
#' @templateVar aka Gaussian
#' @aliases Gaussian
#' @templateVar constructor mean = 0, var = 1, sd = NULL, prec = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab mean, location parameter. \cr
#' @templateVar arg2 \code{var} \tab numeric \tab variance, squared scale parameter. \cr
#' @templateVar arg3 \code{sd} \tab numeric \tab standard deviation, scale parameter. \cr
#' @templateVar arg4 \code{prec} \tab numeric \tab precision, inverse squared scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric, and either \code{var}, \code{sd} or \code{prec} as numerics. These are related via, \deqn{sd = \sqrt(var)}\deqn{prec = 1/var} If \code{prec} is given then \code{sd} and \code{var} are ignored. If \code{sd} is given then \code{var} is ignored.
#'
#' @examples
#' # Different parameterisations
#' Normal$new(var = 1, mean = 1)
#' Normal$new(prec = 2, mean = 1)
#' Normal$new(mean = 1, sd = 2)
#'
#' x <- Normal$new(verbose = TRUE) # Standard normal default
#'
#' # Update parameters
#' x$setParameterValue(var = 2)
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

Normal <- R6Class("Normal", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Normal",
    short_name = "Norm",
    description = "Normal Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(mean = 0, var = 1, sd = NULL, prec = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, var, sd, prec)
      self$setParameterValue(mean = mean, var = var, sd = sd, prec = prec)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
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
      return(self$getParameterValue("mean"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(self$getParameterValue("var"))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      return(0)
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(0)
      } else {
        return(3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      return(0.5 * log(2 * pi * exp(1) * self$getParameterValue("var"), base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      return(exp((self$getParameterValue("mean") * t) + (self$getParameterValue("var") * t^2 * 0.5)))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      return(exp((1i * self$getParameterValue("mean") * t) - (self$getParameterValue("var") * t^2 * 0.5)))
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
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "dnorm",
        x = x,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        log = log,
        vec = test_list(mean)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "pnorm",
        x = x,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(mean)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "qnorm",
        x = p,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(mean)
      )
    },
    .rand = function(n) {
      mean <- self$getParameterValue("mean")
      sd <- self$getParameterValue("sd")
      call_C_base_pdqr(
        fun = "rnorm",
        x = n,
        args = list(
          mean = unlist(mean),
          sd = unlist(sd)
        ),
        vec = test_list(mean)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
      if (!is.null(paramlst$var)) lst <- c(lst, list(var = paramlst$var))
      if (!is.null(paramlst$sd)) lst <- c(lst, list(var = paramlst$sd^2))
      if (!is.null(paramlst$prec)) lst <- c(lst, list(var = paramlst$prec^-1))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table(
    ShortName = "Norm", ClassName = "Normal",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  ))
