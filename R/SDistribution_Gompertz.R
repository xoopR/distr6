
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
Gompertz <- R6Class("Gompertz",
  inherit = SDistribution, lock_objects = F,
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
    initialize = function(shape = 1, scale = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, shape, scale)
      suppressMessages(self$setParameterValue(shape = shape, scale = scale))

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new(zero = T)
      )
    },

    # stats

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      scale <- unlist(self$getParameterValue("scale"))
      shape <- unlist(self$getParameterValue("shape"))

      return((1 / scale) * log((-1 / shape) * log(1 / 2) + 1))
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
    Package = "-", Tags = ""
  )
)
