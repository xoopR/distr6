
#' @name InverseGamma
#' @template SDist
#' @templateVar ClassName InverseGamma
#' @templateVar DistName Inverse Gamma
#' @templateVar uses in Bayesian statistics as the posterior distribution from the unknown variance in a Normal distribution
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha)x^{-\alpha-1}exp(-\beta/x)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{\Gamma} is the gamma function
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{cf}
#' @templateVar additionalDetails The distribution is implemented by interfacing the \code{extraDistr} package.
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link[extraDistr]{InvGamma}} for the d/p/q/r implementation.
#'
#' @examples
#' x <- InverseGamma$new(shape = 1, scale = 4)
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

InverseGamma <- R6Class("InverseGamma", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "InverseGamma",
    short_name = "InvGamma",
    description = "Inverse Gamma Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, scale = 1, decorators = NULL,
                          verbose = FALSE) {

      private$.parameters <- getParameterSet.InverseGamma(self, shape, scale)
      self$setParameterValue(shape = shape, scale = scale)

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
      if (self$getParameterValue("shape") > 1) {
        return(self$getParameterValue("scale") / (self$getParameterValue("shape") - 1))
      } else {
        return(NaN)
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = NULL) {
      return(self$getParameterValue("scale") / (self$getParameterValue("shape") + 1))
    },
    variance = function() {
      if (self$getParameterValue("shape") > 2) {
        return(self$getParameterValue("scale")^2 / ((self$getParameterValue("shape") - 1)^2 * (self$getParameterValue("shape") - 2)))
      } else {
        return(NaN)
      }
    },
    skewness = function() {
      if (self$getParameterValue("shape") > 3) {
        return((4 * sqrt(self$getParameterValue("shape") - 2)) / (self$getParameterValue("shape") - 3))
      } else {
        return(NaN)
      }
    },
    kurtosis = function(excess = TRUE) {
      if (self$getParameterValue("shape") > 4) {
        kur <- (6 * (5 * self$getParameterValue("shape") - 11)) /
          ((self$getParameterValue("shape") - 3) * (self$getParameterValue("shape") - 4))
        if (excess) {
          return(kur)
        } else {
          return(kur + 3)
        }
      } else {
        return(NaN)
      }
    },
    entropy = function(base = 2) {
      return(self$getParameterValue("shape") +
               log(self$getParameterValue("scale") * gamma(self$getParameterValue("shape")), base) -
               (1 + self$getParameterValue("shape")) * digamma(self$getParameterValue("shape")))
    },
    mgf = function(t) {
      return(NaN)
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::dinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dinvgamma(
          x,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::pinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(
            x = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pinvgamma(
          x,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qinvgamma(
          p,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rinvgamma,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rinvgamma(
          n,
          alpha = self$getParameterValue("shape"),
          beta = self$getParameterValue("scale")
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
    ShortName = "InvGamma", ClassName = "InverseGamma",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
