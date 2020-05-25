
#' @name Gompertz
#' @template SDist
#' @templateVar ClassName Gompertz
#' @templateVar DistName Gompertz
#' @templateVar uses in survival analysis particularly to model adult mortality rates.
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \alpha\beta exp(x\beta)exp(\alpha)exp(-exp(x\beta)\alpha)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Non-Negative Reals
#' @templateVar omittedVars \code{mean}, \code{var}, \code{mgf}, \code{cf}, \code{entropy}, \code{skewness} and \code{kurtosis}
#' @templateVar additionalDetails Unfortunately the Gompertz distribution is quite complex to deal with and as such no closed form expressions exist for its mathematical and statistical properties.
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab positive shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab positive scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#'
#' @examples
#' x <- Gompertz$new(shape = 2, scale = 3)
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
#' summary(x)
#' @export
NULL

Gompertz <- R6Class("Gompertz", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Gompertz",
    short_name = "Gomp",
    description = "Gompertz Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, scale = 1, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, shape, scale, verbose)
      suppressMessages(self$setParameterValue(shape = shape, scale = scale))

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new(zero = T)
      )
    },

    # stats
    median = function() {
      scale <- self$getParameterValue("scale")
      shape <- self$getParameterValue("shape")

      return((1/scale) * log((-1/shape) * log(1/2) + 1))
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
          extraDistr::dgompertz,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dgompertz(
          x,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::pgompertz,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pgompertz(
          x,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::qgompertz,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qgompertz(
          p,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(
          extraDistr::rgompertz,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rgompertz(
          n,
          b = self$getParameterValue("shape"),
          a = self$getParameterValue("scale")
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
    ShortName = "Gomp", ClassName = "Gompertz",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "-"
  )
)
