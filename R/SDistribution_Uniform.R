
#' @name Uniform
#' @author Yumi Zhou
#' @template SDist
#' @templateVar ClassName Uniform
#' @templateVar DistName Uniform
#' @templateVar uses to model continuous events occurring with equal probability, as an uninformed prior in Bayesian modelling, and for inverse transform sampling
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(b-a)}
#' @templateVar paramsupport \eqn{-\infty < a < b < \infty}
#' @templateVar distsupport \eqn{[a, b]}
#' @templateVar constructor lower = 0, upper = 1
#' @templateVar arg1 \code{lower} \tab integer \tab lower distribution limit. \cr
#' @templateVar arg2 \code{upper} \tab integer \tab upper distribution limit. \cr
#' @templateVar constructorDets \code{lower} and \code{upper} as numerics.
#'
#' @examples
#' x <- Uniform$new(lower = -10, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(lower = 2, upper = 7)
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

Uniform <- R6Class("Uniform", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Uniform",
    short_name = "Unif",
    description = "Uniform Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, lower, upper, verbose)
      self$setParameterValue(lower = lower, upper = upper)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper),
        symmetry = "sym",
        type = Reals$new()
      )
    },

    # stats
    mean = function() {
      return((self$getParameterValue("lower") + self$getParameterValue("upper")) / 2)
    },
    mode = function(which = NULL) {
      return(NaN)
    },
    variance = function() {
      return(((self$getParameterValue("upper") - self$getParameterValue("lower"))^2) / 12)
    },
    skewness = function() {
      return(0)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(-6 / 5)
      } else {
        return(1.8)
      }
    },
    entropy = function(base = 2) {
      return(log(self$getParameterValue("upper") - self$getParameterValue("lower"), base))
    },
    mgf = function(t) {
      if (t == 0) {
        return(1)
      } else {
        return((exp(self$getParameterValue("upper") * t) - exp(self$getParameterValue("lower") * t)) /
                 (t * (self$getParameterValue("upper") - self$getParameterValue("lower"))))
      }
    },
    cf = function(t) {
      if (t == 0) {
        return(1)
      } else {
        return((exp(self$getParameterValue("upper") * t * 1i) - exp(self$getParameterValue("lower") * t * 1i)) /
                 (t * 1i * (self$getParameterValue("upper") - self$getParameterValue("lower"))))
      }
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
        checkmate::assert(lst[["lower"]] < lst[["upper"]], .var.name = "lower must be < upper")
      } else if ("lower" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] < self$getParameterValue("upper"), .var.name = "lower must be < upper")
      } else if ("upper" %in% names(lst)) {
        checkmate::assert(lst[["upper"]] > self$getParameterValue("lower"), .var.name = "upper must be > lower")
      }

      super$setParameterValue(lst = lst, error = error)
      private$.properties$support <- Interval$new(self$getParameterValue("lower"), self$getParameterValue("upper"))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "dunif",
        x = x,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        log = log,
        vec = test_list(min)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "punif",
        x = x,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(min)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "qunif",
        x = p,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(min)
      )
    },
    .rand = function(n) {
      min <- self$getParameterValue("lower")
      max <- self$getParameterValue("upper")

      call_C_base_pdqr(
        fun = "runif",
        x = n,
        args = list(
          min = unlist(min),
          max = unlist(max)
        ),
        vec = test_list(min)
      )
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
    ShortName = "Unif", ClassName = "Uniform",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
