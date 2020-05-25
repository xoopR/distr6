
#' @name Loglogistic
#' @template SDist
#' @templateVar ClassName Loglogistic
#' @templateVar DistName Log-Logistic
#' @templateVar uses in survival analysis for its non-monotonic hazard as well as in economics
#' @templateVar params shape, \eqn{\beta}, and scale, \eqn{\alpha}
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta/\alpha)(x/\alpha)^{\beta-1}(1 + (x/\alpha)^\beta)^{-2}}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the non-negative Reals
#' @templateVar omittedVars \code{entropy}, \code{mgf} and \code{cf}
#' @templateVar aka Fisk
#' @aliases Fisk
#' @templateVar constructor scale = 1, shape = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link{Logistic}} for the Logistic distribution.
#'
#' @examples
#' x <- Loglogistic$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(scale = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5:6)
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

Loglogistic <- R6Class("Loglogistic", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Loglogistic",
    short_name = "LLogis",
    description = "Loglogistic Probability Distribution.",
    packages = "actuar",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(scale = 1, shape = 1, rate = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, scale, shape, rate)
      self$setParameterValue(scale = scale, shape = shape, rate = rate)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new(zero = T)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return((self$getParameterValue("scale") * pi / self$getParameterValue("shape")) /
               sin(pi / self$getParameterValue("shape")))
    },
    mode = function(which = NULL) {
      shape <- self$getParameterValue("shape")
      return(self$getParameterValue("scale") * ((shape - 1) / (shape + 1))^(1 / shape))
    },
    median = function() {
      return(self$getParameterValue("scale"))
    },
    variance = function() {
      if (self$getParameterValue("shape") > 2) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        return(scale^2 * ((2 * shapi) / sin(2 * shapi) - (shapi^2) / sin(shapi)^2))
      } else {
        return(NaN)
      }
    },
    skewness = function() {
      if (self$getParameterValue("shape") > 3) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        s1 <- (2 * shapi^3 * scale^3) / sin(shapi)^3
        s2 <- (6 * shapi^2 * scale^3) * (1 / sin(shapi)) * (1 / sin(2 * shapi))
        s3 <- (3 * shapi * scale^3) / sin(3 * shapi)
        return(s1 - s2 + s3)
      } else {
        return(NaN)
      }
    },
    kurtosis = function(excess = TRUE) {
      if (self$getParameterValue("shape") > 4) {
        scale <- self$getParameterValue("scale")
        shapi <- pi / self$getParameterValue("shape")
        s1 <- (3 * shapi^4 * scale^4) / sin(shapi)^4
        s2 <- (12 * shapi^3 * scale^4) * (1 / sin(shapi)^2) * (1 / sin(2 * shapi))
        s3 <- (12 * shapi^2 * scale^4) * (1 / sin(shapi)) * (1 / sin(3 * shapi))
        s4 <- (4 * shapi * scale^4) * (1 / sin(4 * shapi))
        kurtosis <- -s1 + s2 - s3 + s4
        if (excess) {
          return(kurtosis - 3)
        } else {
          return(kurtosis)
        }
      } else {
        return(NaN)
      }
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::dllogis,
               shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
               MoreArgs = list(x = x, log = log)
        )
      } else {
        actuar::dllogis(x, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"), log = log)
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::pllogis,
               shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
               MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        actuar::pllogis(x,
                        shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
                        lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::qllogis,
               shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
               MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        actuar::qllogis(p,
                        shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
                        lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("shape"))) {
        mapply(actuar::rllogis,
               shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"),
               MoreArgs = list(n = n)
        )
      } else {
        actuar::rllogis(n, shape = self$getParameterValue("shape"), rate = self$getParameterValue("rate"))
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      if (!is.null(paramlst$rate)) lst <- c(lst, list(scale = paramlst$rate^-1))
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "LLogis", ClassName = "Loglogistic",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "actuar"
  )
)
