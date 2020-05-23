
#' @name Weibull
#' @template SDist
#' @templateVar ClassName Weibull
#' @templateVar DistName Weibull
#' @templateVar uses in survival analysis as it satisfies both PH and AFT requirements
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\alpha/\beta)(x/\beta)^{\alpha-1}exp(-x/\beta)^\alpha}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar constructor shape = 1, scale = 1, altscale = NULL
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{altscale} \tab numeric \tab alternative scale parameter. \cr
#' @templateVar constructorDets \code{shape}, \code{scale}, and \code{altscale} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link{Frechet}} and \code{\link{Gumbel}} for other special cases of the generalized extreme value distribution.
#'
#' @examples
#' # Different parameterisations
#' Weibull$new(shape = 1, scale = 2)
#' Weibull$new(shape = 2, altscale = 2)
#'
#' x <- Weibull$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(scale = 1)
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

Weibull <- R6Class("Weibull", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Weibull",
    short_name = "Weibull",
    description = "Weibull Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize
    initialize = function(shape = 1, scale = 1, altscale = NULL, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, shape, scale, altscale, verbose)
      self$setParameterValue(shape = shape, scale = scale, altscale = altscale)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new(zero = T)
      )
    },

    # stats
    mean = function() {
      return(self$getParameterValue("scale") * gamma(1 + 1 / self$getParameterValue("shape")))
    },
    mode = function(which = NULL) {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")

      if (shape > 1) {
        return(scale * ((shape - 1) / shape)^(1 / shape))
      } else {
        return(0)
      }
    },
    median = function() {
      return(self$getParameterValue("scale") * (log(2)^(1 / self$getParameterValue("shape"))))
    },
    variance = function() {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")
      return(scale^2 * (gamma(1 + 2 / shape) - gamma(1 + 1 / shape)^2))
    },
    skewness = function() {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")
      mu <- self$mean()
      sigma <- self$stdev()
      return(((gamma(1 + 3 / shape) * (scale^3)) - (3 * mu * sigma^2) - (mu^3)) / (sigma^3))
    },
    kurtosis = function(excess = TRUE) {
      skew <- self$skewness()
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")
      mu <- self$mean()
      sigma <- self$stdev()

      kur <- (((scale^4) * gamma(1 + 4 / shape)) - (4 * skew * (sigma^3) * mu) - (6 * (sigma^2) * (mu^2)) - (mu^4)) / (sigma^4)

      if (excess) {
        return(kur - 3)
      } else {
        return(kur)
      }
    },
    entropy = function(base = 2) {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")
      return(-digamma(1) * (1 - 1 / shape) + log(scale / shape, base) + 1)
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "dweibull",
        x = x,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        log = log,
        vec = test_list(shape)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "pweibull",
        x = x,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "qweibull",
        x = p,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .rand = function(n) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      call_C_base_pdqr(
        fun = "rweibull",
        x = n,
        args = list(
          shape = unlist(shape),
          scale = unlist(scale)
        ),
        vec = test_list(shape)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      if (!is.null(paramlst$shape) & !is.null(paramlst$altscale)) {
        lst <- c(lst, list(scale = exp(log(paramlst$altscale) / (-paramlst$shape))))
      }
      if (is.null(paramlst$shape) & !is.null(paramlst$altscale)) {
        lst <- c(lst, list(scale = exp(log(paramlst$altscale) / (-self$getParameterValue("shape")))))
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
    ShortName = "Weibull", ClassName = "Weibull",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
