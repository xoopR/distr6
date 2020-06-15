#' @name Pareto
#' @template SDist
#' @templateVar ClassName Pareto
#' @templateVar DistName Pareto
#' @templateVar uses in Economics to model the distribution of wealth and the 80-20 rule
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha\beta^\alpha)/(x^{\alpha+1})}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport \eqn{[\beta, \infty)}
#' @details
#' Currently this is implemented as the Type I Pareto distribution, other types
#' will be added in the future.
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
#' @template param_shape
#' @template param_scale
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Pareto <- R6Class("Pareto",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Pareto",
    short_name = "Pare",
    description = "Pareto (Type I) Probability Distribution.",
    packages = c("extraDistr", "expint"),

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, scale = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, shape, scale)
      self$setParameterValue(shape = shape, scale = scale)

      super$initialize(
        decorators = decorators,
        support = Interval$new(scale, Inf, type = "[)"),
        type = PosReals$new(zero = T)
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
        return((self$getParameterValue("shape") * self$getParameterValue("scale")) / (self$getParameterValue("shape") - 1))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      return(self$getParameterValue("scale"))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      return(self$getParameterValue("scale") * 2^(1 / self$getParameterValue("shape")))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")
      if (shape <= 2) {
        return(Inf)
      } else {
        return((shape * scale^2) / ((shape - 1)^2 * (shape - 2)))
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      shape <- self$getParameterValue("shape")
      if (shape > 3) {
        return(((2 * (1 + shape)) / (shape - 3)) * sqrt((shape - 2) / shape))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      shape <- self$getParameterValue("shape")
      if (shape > 4) {
        kur <- (6 * (shape^3 + shape^2 - 6 * shape - 2)) / (shape * (shape - 3) * (shape - 4))
      } else {
        return(NaN)
      }

      if (excess) {
        return(kur)
      } else {
        return(kur + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      return(log((scale / shape) * exp(1 + 1 / shape), base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      if (t < 0) {
        shape <- self$getParameterValue("shape")
        scale <- self$getParameterValue("scale")
        return(shape * (-scale * t)^shape * expint::gammainc(-shape, -scale * t))
      } else {
        return(NaN)
      }
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
      private$.properties$support <- Interval$new(self$getParameterValue("scale"), Inf, type = "[)")
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::dpareto,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dpareto(
          x,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::ppareto,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::ppareto(
          x,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qpareto,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qpareto(
          p,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rpareto,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rpareto(
          n,
          a = self$getParameterValue("shape"),
          b = self$getParameterValue("scale")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
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
    ShortName = "Pare", ClassName = "Pareto",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "locscale"
  )
)
