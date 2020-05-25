
#' @name Cauchy
#' @author Chijing Zeng
#' @template SDist
#' @templateVar ClassName Cauchy
#' @templateVar DistName Cauchy
#' @templateVar uses in physics and finance
#' @templateVar params location, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1 / (\pi\beta(1 + ((x - \alpha) / \beta)^2))}
#' @templateVar paramsupport \eqn{\alpha \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar additionalDetails The mean and variance are undefined, hence \code{NaN} is returned.
#' @templateVar constructor location = 0, scale = 1
#' @templateVar arg1 \code{location} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{location} as a numeric and \code{scale} as a positive numeric.
#'
#' @examples
#' x <- Cauchy$new(location = 2, scale = 5)
#'
#' # Update parameters
#' x$setParameterValue(scale = 3)
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

Cauchy <- R6Class("Cauchy", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Cauchy",
    short_name = "Cauchy",
    description = "Cauchy Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(location = 0, scale = 1,
                          decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, location, scale, verbose)
      self$setParameterValue(location = location, scale = scale)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
      )
    },

    # stats
    mean = function() {
      return(NaN)
    },
    mode = function(which = NULL) {
      return(self$getParameterValue("location"))
    },
    variance = function() {
      return(NaN)
    },
    skewness = function() {
      return(NaN)
    },
    kurtosis = function(excess = TRUE) {
      return(NaN)
    },
    entropy = function(base = 2) {
      return(log(4 * pi * self$getParameterValue("scale"), base))
    },
    mgf = function(t) {
      return(NaN)
    },
    cf = function(t) {
      return(exp((self$getParameterValue("location") * 1i * t) - (self$getParameterValue("scale") * abs(t))))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "dcauchy",
        x = x,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        log = log,
        vec = test_list(location)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "pcauchy",
        x = x,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(location)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "qcauchy",
        x = p,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(location)
      )
    },
    .rand = function(n) {
      location <- self$getParameterValue("location")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "rcauchy",
        x = n,
        args = list(
          location = unlist(location),
          scale = unlist(scale)
        ),
        vec = test_list(location)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$location)) lst <- c(lst, list(location = paramlst$location))
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
    ShortName = "Cauchy", ClassName = "Cauchy",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
