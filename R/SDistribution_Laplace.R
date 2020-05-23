
#' @name Laplace
#' @template SDist
#' @templateVar ClassName Laplace
#' @templateVar DistName Laplace
#' @templateVar uses in signal processing and finance
#' @templateVar params mean, \eqn{\mu}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-|x-\mu|/\beta)/(2\beta)}
#' @templateVar paramsupport \eqn{\mu \epsilon R} and \eqn{\beta > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor mean = 0, scale = 1, var = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar arg3 \code{var} \tab numeric \tab alternate scale parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric and either \code{scale} or \code{var} as positive numerics. These are related via, \deqn{var = 2 * scale^2} If \code{var} is given then {scale} is ignored.
#'
#' @examples
#' Laplace$new(scale = 2)
#' Laplace$new(var = 4)
#'
#' x <- Laplace$new(verbose = TRUE) # Default is mean = 0, scale = 1
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(var = 2)
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

Laplace <- R6Class("Laplace", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Laplace",
    short_name = "Lap",
    description = "Laplace Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize
    initialize = function(mean = 0, scale = 1, var = NULL,
                          decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, mean, scale, var, verbose)
      self$setParameterValue(mean = mean, scale = scale, var = var)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
      )
    },

    # stats
    mean = function() {
      self$getParameterValue("mean")
    },
    mode = function(which = NULL) {
      return(self$getParameterValue("mean"))
    },
    variance = function() {
      self$getParameterValue("var")
    },
    skewness = function() {
      return(0)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(3)
      } else {
        return(6)
      }
    },
    entropy = function(base = 2) {
      return(log(2 * exp(1) * self$getParameterValue("scale"), base))
    },
    mgf = function(t) {
      if (abs(t) < 1 / self$getParameterValue("scale")) {
        return(exp(self$getParameterValue("mean") * t) / (1 - self$getParameterValue("scale")^2 * t^2))
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      return(exp(self$getParameterValue("mean") * t * 1i) / (1 + self$getParameterValue("scale")^2 * t^2))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::dlaplace,
               mu = self$getParameterValue("mean"),
               sigma = self$getParameterValue("scale"),
               MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dlaplace(x,
                             mu = self$getParameterValue("mean"),
                             sigma = self$getParameterValue("scale"),
                             log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::plaplace,
               mu = self$getParameterValue("mean"),
               sigma = self$getParameterValue("scale"),
               MoreArgs = list(q = x, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::plaplace(x,
                             mu = self$getParameterValue("mean"),
                             sigma = self$getParameterValue("scale"),
                             lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::qlaplace,
               mu = self$getParameterValue("mean"),
               sigma = self$getParameterValue("scale"),
               MoreArgs = list(p = p, lower.tail = lower.tail, log.p = log.p)
        )
      } else {
        extraDistr::qlaplace(p,
                             mu = self$getParameterValue("mean"),
                             sigma = self$getParameterValue("scale"),
                             lower.tail = lower.tail, log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("mean"))) {
        mapply(extraDistr::rlaplace,
               mu = self$getParameterValue("mean"),
               sigma = self$getParameterValue("scale"),
               MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rlaplace(n,
                             mu = self$getParameterValue("mean"),
                             sigma = self$getParameterValue("scale")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
      if (!is.null(paramlst$scale)) lst <- c(lst, list(scale = paramlst$scale))
      if (!is.null(paramlst$var)) lst <- c(lst, list(scale = sqrt(paramlst$var / 2)))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Lap", ClassName = "Laplace",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
