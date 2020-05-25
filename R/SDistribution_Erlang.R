
#' @name Erlang
#' @template SDist
#' @templateVar ClassName Erlang
#' @templateVar DistName Erlang
#' @templateVar uses as a special case of the Gamma distribution when the shape parameter is an integer
#' @templateVar params shape, \eqn{\alpha}, and rate, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)(x^{\alpha-1})(exp(-x\beta)) /(\alpha-1)!}
#' @templateVar paramsupport \eqn{\alpha = 1,2,3,\ldots} and \eqn{\beta > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor shape = 1, rate = 1, scale = NULL
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{rate} \tab numeric \tab inverse scale parameter. \cr
#' @templateVar arg3 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and either \code{rate} or \code{scale}, all as positive numerics. These are related via, \deqn{scale = 1/rate} If \code{scale} is given then \code{rate} is ignored.
#'
#' @examples
#' Erlang$new(shape = 1, rate = 2)
#' Erlang$new(shape = 1, scale = 4)
#'
#' # Default is shape = 1, rate = 1
#' x <- Erlang$new(verbose = TRUE)
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

Erlang <- R6Class("Erlang", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Erlang",
    short_name = "Erlang",
    description = "Erlang Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape = 1, rate = 1, scale = NULL, decorators = NULL,
                          verbose = FALSE) {

      private$.parameters <- getParameterSet.Erlang(self, shape, rate, scale, verbose)
      self$setParameterValue(shape = shape, rate = rate, scale = scale)

      super$initialize(
        decorators = decorators,
        support = PosReals$new(zero = T),
        type = PosReals$new()
      )
    },

    # stats
    mean = function() {
      self$getParameterValue("shape") / self$getParameterValue("rate")
    },
    mode = function(which = NULL) {
      (self$getParameterValue("shape") - 1) / self$getParameterValue("rate")
    },
    variance = function() {
      self$getParameterValue("shape") / (self$getParameterValue("rate")^2)
    },
    skewness = function() {
      2 / sqrt(self$getParameterValue("shape"))
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(6 / self$getParameterValue("shape"))
      } else {
        return((6 / self$getParameterValue("shape")) + 3)
      }
    },
    entropy = function(base = 2) {
      (1 - self$getParameterValue("shape")) * digamma(self$getParameterValue("shape")) +
        self$getParameterValue("shape") +
        log(gamma(self$getParameterValue("shape") / self$getParameterValue("rate")), base)
    },
    mgf = function(t) {
      if (t < self$getParameterValue("rate")) {
        return((1 - self$getParameterValue("scale") * t)^(-self$getParameterValue("shape")))
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      (1 - self$getParameterValue("scale") * 1i * t)^(-self$getParameterValue("shape"))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "dgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        log = log,
        vec = test_list(shape)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "pgamma",
        x = x,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "qgamma",
        x = p,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape)
      )
    },
    .rand = function(n) {
      shape <- self$getParameterValue("shape")
      rate <- self$getParameterValue("rate")
      call_C_base_pdqr(
        fun = "rgamma",
        x = n,
        args = list(
          shape = unlist(shape),
          rate = unlist(rate)
        ),
        vec = test_list(shape)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$shape)) lst <- c(lst, list(shape = paramlst$shape))
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
    ShortName = "Erlang", ClassName = "Erlang",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
