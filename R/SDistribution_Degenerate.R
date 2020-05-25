
#' @name Degenerate
#' @template SDist
#' @templateVar ClassName Degenerate
#' @templateVar DistName Degenerate
#' @templateVar uses to model deterministic events or as a representation of the delta, or Heaviside, function
#' @templateVar params mean, \eqn{\mu}
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1, \ if \ x = \mu}{f(x) = 1, if x = \mu}\deqn{f(x) = 0, \ if \ x \neq \mu}{f(x) = 0, if x != \mu}
#' @templateVar paramsupport \eqn{\mu \epsilon R}
#' @templateVar distsupport \eqn{{\mu}}
#' @templateVar aka Dirac
#' @aliases Dirac Delta
#' @templateVar constructor mean = 0
#' @templateVar arg1 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar constructorDets \code{mean} as a numeric.
#'
#' @examples
#' x <- Degenerate$new(mean = 4)
#'
#' # Update parameters
#' x$setParameterValue(mean = 2.56)
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

Degenerate <- R6Class("Degenerate", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Degenerate",
    short_name = "Degen",
    description = "Degenerate Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(mean = 0, decorators = NULL) {

      private$.parameters <- getParameterSet(self, mean)
      self$setParameterValue(mean = mean)

      super$initialize(
        decorators = decorators,
        support = Set$new(mean, class = "integer"),
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
    mode = function(which = NULL) {
      return(self$getParameterValue("mean"))
    },
    variance = function() {
      return(0)
    },
    skewness = function() {
      return(NaN)
    },
    kurtosis = function(excess = TRUE) {
      return(NaN)
    },
    entropy = function(base = 2) {
      return(0)
    },
    mgf = function(t) {
      return(exp(self$getParameterValue("mean") * t))
    },
    cf = function(t) {
      return(exp(self$getParameterValue("mean") * t * 1i))
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      private$.properties$support <- Set$new(self$getParameterValue("mean"))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegeneratePdf(x, unlist(mean), log))
      } else {
        return(as.numeric(C_DegeneratePdf(x, mean, log)))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegenerateCdf(x, unlist(mean), lower.tail, log.p))
      } else {
        return(as.numeric(C_DegenerateCdf(x, mean, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      mean <- self$getParameterValue("mean")

      if (checkmate::testList(mean)) {
        return(C_DegenerateQuantile(p, unlist(mean), lower.tail, log.p))
      } else {
        return(as.numeric(C_DegenerateQuantile(p, mean, lower.tail, log.p)))
      }
    },
    .rand = function(n) {
      rep(self$getParameterValue("mean"), n)
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Degen", ClassName = "Degenerate",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-"
  )
)
