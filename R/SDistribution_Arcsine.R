#' @name Arcsine
#' @template SDist
#' @templateVar ClassName Arcsine
#' @templateVar DistName Arcsine
#' @templateVar uses in the study of random walks and as a special case of the Beta distribution
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(\pi\sqrt{(x-a)(b-x))}}
#' @templateVar paramsupport \eqn{-\infty < a \le b < \infty}
#' @templateVar distsupport \eqn{[a, b]}
#' @templateVar omittedVars \code{cf} and \code{mgf}
#' @templateVar additionalDetails When the Standard Arcsine is constructed (default) then \code{\link[stats]{rbeta}} is used for sampling, otherwise via inverse transform
#' @templateVar constructor lower = 0, upper = 1
#' @templateVar arg1 \code{lower} \tab integer \tab lower distribution limit. \cr
#' @templateVar arg2 \code{upper} \tab integer \tab upper distribution limit. \cr
#' @templateVar constructorDets \code{lower} and \code{upper} as numerics.
#' @templateVar additionalSeeAlso \code{\link{rbeta}} for the Beta distribution sampling function.
#'
#' @template param_decorators
#' @template param_lower
#' @template param_upper
#' @template class_distribution
#' @template method_setParameterValue
#' @template method_mode
#' @template method_kurtosis
#' @template method_entropy
#'
#' @export
NULL

Arcsine <- R6Class("Arcsine", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Arcsine",
    short_name = "Arc",
    description = "Arcsine Probability Distribution.",

    # Public methods
    # initialize
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(lower = 0, upper = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, lower, upper)
      self$setParameterValue(lower = lower, upper = upper)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper),
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
      return((self$getParameterValue("upper") + self$getParameterValue("lower")) / 2)
    },
    mode = function(which = "all") {
      if (which == "all") {
        return(c(self$getParameterValue("lower"), self$getParameterValue("upper")))
      } else {
        return(c(self$getParameterValue("lower"), self$getParameterValue("upper"))[which])
      }
    },
    variance = function() {
      return(((self$getParameterValue("upper") - self$getParameterValue("lower"))^2) / 8)
    },
    skewness = function() {
      return(0)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(-3 / 2)
      } else {
        return(1.5)
      }
    },
    entropy = function(base = 2) {
      return(log(pi / 4, base))
    },
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if ("lower" %in% names(lst) & "upper" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] <= lst[["upper"]])
      } else if ("lower" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"))
      } else if ("upper" %in% names(lst)) {
        checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"))
      }

      super$setParameterValue(lst = lst, error = error)
      private$.properties$support <- Interval$new(self$getParameterValue("lower"), self$getParameterValue("upper"))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsinePdf(x, unlist(lower), unlist(upper), log))
      } else {
        return(as.numeric(C_ArcsinePdf(x, lower, upper, log)))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsineCdf(x, unlist(lower), unlist(upper), lower.tail, log.p))
      } else {
        return(as.numeric(C_ArcsineCdf(x, lower, upper, lower.tail, log.p)))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")

      if (checkmate::testList(lower)) {
        return(C_ArcsineQuantile(x, unlist(lower), unlist(upper), lower.tail, log.p))
      } else {
        return(as.numeric(C_ArcsineQuantile(x, lower, upper, lower.tail, log.p)))
      }
    },
    .rand = function(n) {
      self$quantile(runif(n))
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$lower)) lst <- c(lst, list(lower = paramlst$lower))
      if (!is.null(paramlst$upper)) lst <- c(lst, list(upper = paramlst$upper))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Arc", ClassName = "Arcsine",
    Type = "\u211D", ValueSupport = "continuous", VariateForm = "univariate",
    Package = "-"
  )
)
