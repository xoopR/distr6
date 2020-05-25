
#' @name Exponential
#' @template SDist
#' @templateVar ClassName Exponential
#' @templateVar DistName Exponential
#' @templateVar uses to model inter-arrival times in a Poisson process and has the memoryless property
#' @templateVar params rate, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \lambda exp(-x\lambda)}
#' @templateVar paramsupport \eqn{\lambda > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor rate = NULL, scale = NULL
#' @templateVar arg1 \code{rate} \tab numeric \tab arrival rate. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets  \code{rate} or \code{scale} as positive numerics. These are related via, \deqn{scale = 1/rate} If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Exponential$new(rate = 4)
#' Exponential$new(scale = 3)
#'
#' x <- Exponential$new(verbose = TRUE) # Default is rate = 1
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

Exponential <- R6Class("Exponential", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Exponential",
    short_name = "Exp",
    description = "Exponential Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(rate = 1, scale = NULL, decorators = NULL) {

      private$.parameters <- getParameterSet(self, rate, scale)
      self$setParameterValue(rate = rate, scale = scale)

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
      self$getParameterValue("scale")
    },
    mode = function(which = NULL) {
      return(0)
    },
    median = function() {
      self$getParameterValue("scale") * log(2)
    },
    variance = function() {
      self$getParameterValue("scale")^2
    },
    skewness = function() {
      return(2)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(6)
      } else {
        return(9)
      }
    },
    entropy = function(base = 2) {
      1 - log(self$getParameterValue("rate"), base)
    },
    mgf = function(t) {
      if (t < self$getParameterValue("rate")) {
        return(self$getParameterValue("rate") / (self$getParameterValue("rate") - t))
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      return(self$getParameterValue("rate") / (self$getParameterValue("rate") - ((0 + 1i) * t)))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dexp",
        x = x,
        args = list(rate = unlist(rate)),
        log = log,
        vec = test_list(rate)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "pexp",
        x = x,
        args = list(rate = unlist(rate)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(rate)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qexp",
        x = p,
        args = list(rate = unlist(rate)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(rate)
      )
    },
    .rand = function(n) {
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rexp",
        x = n,
        args = list(rate = unlist(rate)),
        vec = test_list(rate)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$rate)) lst <- c(lst, list(rate = paramlst$rate))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(rate = paramlst$scale^-1))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Exp", ClassName = "Exponential",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
