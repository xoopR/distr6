
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
#' @templateVar omittedVars \code{cf}
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#' @templateVar additionalDetails Currently this is implemented as the Type I Pareto distribution, other types
#' will be added in the future.
#'
#' @examples
#' x <- Pareto$new(shape = 2, scale = 1)
#'
#' # Update parameters
#' x$setParameterValue(scale = 5.1)
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

Pareto <- R6Class("Pareto", inherit = SDistribution, lock_objects = F,
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
    initialize = function(shape = 1, scale = 1, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, shape, scale, verbose)
      self$setParameterValue(shape = shape, scale = scale)

      super$initialize(
        decorators = decorators,
        support = Interval$new(scale, Inf, type = "[)"),
        type = PosReals$new(zero = T)
      )
    },

    # stats
    mean = function() {
      if (self$getParameterValue("shape") <= 1) {
        return(Inf)
      } else {
        return((self$getParameterValue("shape") * self$getParameterValue("scale")) / (self$getParameterValue("shape") - 1))
      }
    },
    mode = function(which = NULL) {
      return(self$getParameterValue("scale"))
    },
    median = function() {
      return(self$getParameterValue("scale") * 2^(1/self$getParameterValue("shape")))
    },
    variance = function() {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")
      if (shape <= 2) {
        return(Inf)
      } else {
        return((shape * scale^2) / ((shape - 1)^2 * (shape - 2)))
      }
    },
    skewness = function() {
      shape <- self$getParameterValue("shape")
      if (shape > 3) {
        return(((2 * (1 + shape)) / (shape - 3)) * sqrt((shape - 2) / shape))
      } else {
        return(NaN)
      }
    },
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
    entropy = function(base = 2) {
      shape <- self$getParameterValue("shape")
      scale <- self$getParameterValue("scale")

      return(log((scale / shape) * exp(1 + 1 / shape), base))
    },
    mgf = function(t) {
      if (t < 0) {
        shape <- self$getParameterValue("shape")
        scale <- self$getParameterValue("scale")
        return(shape * (-scale * t)^shape * expint::gammainc(-shape, -scale * t))
      } else {
        return(NaN)
      }
    },
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
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
    Package = "extraDistr"
  )
)
