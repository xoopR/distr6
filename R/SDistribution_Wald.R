
#' @name Wald
#' @template SDist
#' @templateVar ClassName Wald
#' @templateVar DistName Wald
#' @templateVar uses for modelling the first passage time for Brownian motion
#' @templateVar params mean, \eqn{\mu}, and shape, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\lambda/(2x^3\pi))^{1/2} exp((-\lambda(x-\mu)^2)/(2\mu^2x))}
#' @templateVar paramsupport \eqn{\lambda > 0} and \eqn{\mu > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{entropy}
#' @templateVar omittedDPQR \code{quantile}
#' @templateVar aka Inverse Normal
#' @aliases InverseNormal InverseGaussian
#' @templateVar additionalDetails Sampling is performed as per Michael, Schucany, Haas (1976).
#' @templateVar constructor mean = 1, shape = 1
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar constructorDets \code{mean} and \code{shape} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution.
#'
#' @examples
#' x <- Wald$new(mean = 2, shape = 5)
#'
#' # Update parameters
#' x$setParameterValue(shape = 3)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
NULL

Wald <- R6Class("Wald", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Wald",
    short_name = "Wald",
    description = "Wald Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(mean = 1, shape = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, shape)
      self$setParameterValue(mean = mean, shape = shape)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(),
        symmetry = "sym",
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
    mode = function(which = NULL) {
      mean <- self$getParameterValue("mean")
      shape <- self$getParameterValue("shape")
      return(mean * ((1 + (9 * mean^2) / (4 * shape^2))^0.5 - (3 * mean) / (2 * shape)))
    },
    variance = function() {
      return(self$getParameterValue("mean")^3 / self$getParameterValue("shape"))
    },
    skewness = function() {
      return(3 * (self$getParameterValue("mean") / self$getParameterValue("shape"))^0.5)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(15 * self$getParameterValue("mean") / self$getParameterValue("shape"))
      } else {
        return(15 * self$getParameterValue("mean") / self$getParameterValue("shape") + 3)
      }
    },
    mgf = function(t) {
      mean <- self$getParameterValue("mean")
      shape <- self$getParameterValue("shape")
      return(exp(shape / mean * (1 - sqrt(1 - 2 * mean^2 * t / shape))))
    },
    cf = function(t) {
      mean <- self$getParameterValue("mean")
      shape <- self$getParameterValue("shape")
      return(exp(shape / mean * (1 - sqrt(1 - 2 * mean^2 * 1i * t / shape))))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::dwald,
               mu = self$getParameterValue("mean"),
               lambda = self$getParameterValue("shape"),
               MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dwald(x,
                          mu = self$getParameterValue("mean"),
                          lambda = self$getParameterValue("shape"),
                          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(
          extraDistr::pwald,
          mu = self$getParameterValue("mean"),
          lambda = self$getParameterValue("shape"),
          MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::pwald(x,
                          mu = self$getParameterValue("mean"),
                          lambda = self$getParameterValue("shape"),
                          lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::rwald,
               mu = self$getParameterValue("mean"),
               lambda = self$getParameterValue("shape"),
               MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rwald(n,
                          mu = self$getParameterValue("mean"),
                          lambda = self$getParameterValue("shape")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
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
    ShortName = "Wald", ClassName = "Wald",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
