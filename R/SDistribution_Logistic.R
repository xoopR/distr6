
#' @name Logistic
#' @template SDist
#' @templateVar ClassName Logistic
#' @templateVar DistName Logistic
#' @templateVar uses in logistic regression and feedforward neural networks
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{s},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-(x-\mu)/s) / (s(1+exp(-(x-\mu)/s))^2)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{s > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor mean = 0, scale = 1, sd = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{sd} \tab numeric \tab standard deviation, alternate scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric and either \code{scale} or \code{sd} as positive numerics. These are related via, \deqn{sd = scale*\pi/\sqrt(3)} If \code{sd} is given then {scale} is ignored.
#'
#' @examples
#' x <- Logistic$new(mean = 2, scale = 3)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(sd = 2)
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

Logistic <- R6Class("Logistic", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Logistic",
    short_name = "Logis",
    description = "Logistic Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(mean = 0, scale = 1, sd = NULL,
                          decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean, scale, sd)
      self$setParameterValue(mean = mean, scale = scale, sd = sd)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
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

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = NULL) {
      return(self$getParameterValue("mean"))
    },
    variance = function() {
      return(self$getParameterValue("sd")^2)
    },
    skewness = function() {
      return(0)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(6 / 5)
      } else {
        return(6 / 5 + 3)
      }
    },
    entropy = function(base = 2) {
      return(2 + log(self$getParameterValue("scale"), base))
    },
    mgf = function(t) {
      if (-1 / self$getParameterValue("scale") < t & t < 1 / self$getParameterValue("scale")) {
        return(exp(self$getParameterValue("mean") * t) * beta(1 - self$getParameterValue("scale") * t, 1 + self$getParameterValue("scale") * t))
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      return(exp(1i * self$getParameterValue("mean") * t) *
               (self$getParameterValue("scale") * pi * t) / (sinh(pi * self$getParameterValue("scale") * t)))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "dlogis",
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
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "plogis",
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
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "qlogis",
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
      location <- self$getParameterValue("mean")
      scale <- self$getParameterValue("scale")
      call_C_base_pdqr(
        fun = "rlogis",
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
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      if (!is.null(paramlst$sd)) lst <- c(lst, list(scale = paramlst$sd * sqrt(3) / pi))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Logis", ClassName = "Logistic",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
