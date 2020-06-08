
#' @name Lognormal
#' @template SDist
#' @templateVar ClassName Lognormal
#' @templateVar DistName Log-Normal
#' @templateVar uses to model many natural phenomena as a result of growth driven by small percentage changes
#' @templateVar params logmean, \eqn{\mu}, and logvar, \eqn{\sigma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{exp(-(log(x)-\mu)^2/2\sigma^2)/(x\sigma\sqrt(2\pi))}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\sigma > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar aka Log-Gaussian
#' @aliases Loggaussian
#'
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
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Lognormal <- R6Class("Lognormal",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Lognormal",
    short_name = "Lnorm",
    description = "Lognormal Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param meanlog `(numeric(1))`\cr
    #' Mean of the distribution on the log scale, defined on the Reals.
    #' @param varlog `(numeric(1))`\cr
    #' Variance of the distribution on the log scale, defined on the positive Reals.
    #' @param sdlog `(numeric(1))`\cr
    #' Standard deviation of the distribution on the log scale, defined on the positive Reals.
    #' \deqn{sdlog = varlog^2}. If `preclog` missing and `sdlog` given then all other parameters except
    #' `meanlog` are ignored.
    #' @param preclog `(numeric(1))`\cr
    #' Precision of the distribution on the log scale, defined on the positive Reals.
    #' \deqn{preclog = 1/varlog}. If given then all other parameters except `meanlog` are ignored.
    #' @param mean `(numeric(1))`\cr
    #' Mean of the distribution on the natural scale, defined on the positive Reals.
    #' @param var `(numeric(1))`\cr
    #' Variance of the distribution on the natural scale, defined on the positive Reals.
    #' \deqn{var = (exp(var) - 1)) * exp(2 * meanlog + varlog)}
    #' @param sd `(numeric(1))`\cr
    #' Standard deviation of the distribution on the natural scale, defined on the positive Reals.
    #' \deqn{sd = var^2}. If `prec` missing and `sd` given then all other parameters except
    #' `mean` are ignored.
    #' @param prec `(numeric(1))`\cr
    #' Precision of the distribution on the natural scale, defined on the Reals.
    #' \deqn{prec = 1/var}. If given then all other parameters except `mean` are ignored.
    #' @examples
    #' Lognormal$new(var = 2, mean = 1)
    #' Lognormal$new(meanlog = 2, preclog = 5)
    initialize = function(meanlog = 0, varlog = 1, sdlog = NULL, preclog = NULL,
                          mean = 1, var = NULL, sd = NULL, prec = NULL,
                          decorators = NULL) {

      if (!is.null(var) | !is.null(sd) | !is.null(prec)) {
        meanlog <- varlog <- sdlog <- preclog <- NULL
      }

      private$.parameters <- getParameterSet(self, meanlog, varlog, sdlog, preclog, mean, var, sd, prec)
      self$setParameterValue(
        meanlog = meanlog, varlog = varlog, sdlog = sdlog, preclog = preclog,
        mean = mean, var = var, sd = sd, prec = prec
      )

      super$initialize(
        decorators = decorators,
        support = PosReals$new(),
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
    mode = function(which = "all") {
      return(exp(self$getParameterValue("meanlog") - self$getParameterValue("varlog")))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      return(exp(self$getParameterValue("meanlog")))
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
      return(sqrt(exp(self$getParameterValue("varlog")) - 1) * (exp(self$getParameterValue("varlog")) + 2))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return((exp(4 * self$getParameterValue("varlog")) + 2 * exp(3 * self$getParameterValue("varlog")) +
          3 * exp(2 * self$getParameterValue("varlog")) - 6))
      } else {
        return((exp(4 * self$getParameterValue("varlog")) + 2 * exp(3 * self$getParameterValue("varlog")) +
          3 * exp(2 * self$getParameterValue("varlog")) - 3))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      return(log(sqrt(2 * pi) * self$getParameterValue("sdlog") *
        exp(self$getParameterValue("meanlog") + 0.5), base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      return(NaN)
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
      meanlog <- self$getParameterValue("meanlog")
      sdlog <- self$getParameterValue("sdlog")
      call_C_base_pdqr(
        fun = "dlnorm",
        x = x,
        args = list(
          meanlog = unlist(meanlog),
          sdlog = unlist(sdlog)
        ),
        log = log,
        vec = test_list(meanlog)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      meanlog <- self$getParameterValue("meanlog")
      sdlog <- self$getParameterValue("sdlog")
      call_C_base_pdqr(
        fun = "plnorm",
        x = x,
        args = list(
          meanlog = unlist(meanlog),
          sdlog = unlist(sdlog)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(meanlog)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      meanlog <- self$getParameterValue("meanlog")
      sdlog <- self$getParameterValue("sdlog")
      call_C_base_pdqr(
        fun = "qlnorm",
        x = p,
        args = list(
          meanlog = unlist(meanlog),
          sdlog = unlist(sdlog)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(meanlog)
      )
    },
    .rand = function(n) {
      meanlog <- self$getParameterValue("meanlog")
      sdlog <- self$getParameterValue("sdlog")
      call_C_base_pdqr(
        fun = "rlnorm",
        x = n,
        args = list(
          meanlog = unlist(meanlog),
          sdlog = unlist(sdlog)
        ),
        vec = test_list(meanlog)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()

      if (!is.null(paramlst$meanlog)) {
        meanlog <- paramlst$meanlog
      } else {
        meanlog <- self$getParameterValue("meanlog")
      }
      if (!is.null(paramlst$varlog)) {
        varlog <- paramlst$varlog
      } else {
        varlog <- self$getParameterValue("varlog")
      }
      if (!is.null(paramlst$mean)) {
        mean <- paramlst$mean
      } else {
        mean <- self$getParameterValue("mean")
      }
      if (!is.null(paramlst$var)) {
        var <- paramlst$var
      } else {
        var <- self$getParameterValue("var")
      }

      if (self$parameters("meanlog")$settable) {
        if (!is.null(paramlst[["meanlog"]])) lst <- c(lst, list(meanlog = paramlst$meanlog))
        if (!is.null(paramlst[["varlog"]])) lst <- c(lst, list(varlog = paramlst$varlog))
        if (!is.null(paramlst[["sdlog"]])) lst <- c(lst, list(varlog = paramlst$sdlog^2))
        if (!is.null(paramlst[["preclog"]])) lst <- c(lst, list(varlog = paramlst$preclog^-1))
      } else {
        if (!is.null(paramlst[["mean"]])) lst <- c(lst, list(mean = paramlst$mean))
        if (!is.null(paramlst[["var"]])) lst <- c(lst, list(var = paramlst$var))
        if (!is.null(paramlst[["sd"]])) lst <- c(lst, list(var = paramlst$sd^2))
        if (!is.null(paramlst[["prec"]])) lst <- c(lst, list(var = paramlst$prec^-1))
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
    ShortName = "Lnorm", ClassName = "Lognormal",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
