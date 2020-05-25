
#' @name Poisson
#' @template SDist
#' @templateVar ClassName Poisson
#' @templateVar DistName Poisson
#' @templateVar uses to model the number of events occurring in at a constant, independent rate over an interval of time or space
#' @templateVar params arrival rate, \eqn{\lambda},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (\lambda^x * exp(-\lambda))/x!}
#' @templateVar paramsupport \eqn{\lambda} > 0
#' @templateVar distsupport the Naturals
#' @templateVar omittedVars \code{entropy}
#' @templateVar constructor rate = 1
#' @templateVar arg1 \code{rate} \tab numeric \tab arrival rate. \cr
#' @templateVar constructorDets \code{rate} as a positive numeric.
#'
#'
#' @examples
#' x <- Poisson$new(rate = 2)
#'
#' # Update parameters
#' x$setParameterValue(rate = 3)
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

Poisson <- R6Class("Poisson", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Poisson",
    short_name = "Pois",
    description = "Poisson Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(rate = 1, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, rate, verbose)
      self$setParameterValue(rate = rate)

      super$initialize(
        decorators = decorators,
        support = Naturals$new(),
        type = Naturals$new()
      )
    },

    # stats
    mean = function() {
      return(self$getParameterValue("rate"))
    },
    mode = function(which = NULL) {
      return(floor(self$getParameterValue("rate")))
    },
    variance = function() {
      return(self$getParameterValue("rate"))
    },
    skewness = function() {
      return(self$getParameterValue("rate")^(-0.5))
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(1 / self$getParameterValue("rate"))
      } else {
        return(1 / self$getParameterValue("rate") + 3)
      }
    },
    mgf = function(t) {
      return(exp(self$getParameterValue("rate") * (exp(t) - 1)))
    },
    cf = function(t) {
      return(exp(self$getParameterValue("rate") * (exp(1i * t) - 1)))
    },
    pgf = function(z) {
      return(exp(self$getParameterValue("rate") * (z - 1)))
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dpois",
        x = x,
        args = list(lambda = unlist(lambda)),
        log = log,
        vec = test_list(lambda)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "ppois",
        x = x,
        args = list(lambda = unlist(lambda)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(lambda)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qpois",
        x = p,
        args = list(lambda = unlist(lambda)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(lambda)
      )
    },
    .rand = function(n) {
      lambda <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rpois",
        x = n,
        args = list(lambda = unlist(lambda)),
        vec = test_list(lambda)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$rate)) lst <- c(lst, list(rate = paramlst$rate))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Pois", ClassName = "Poisson",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats"
  )
)
