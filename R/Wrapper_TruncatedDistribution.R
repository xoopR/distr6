#' @name TruncatedDistribution
#' @title Distribution Truncation Wrapper
#' @description A wrapper for truncating any probability distribution at given limits.
#' @template class_wrapper
#' @template class_trunchub
#' @template method_setParameterValue
#'
#' @details
#' Truncates a distribution at lower and upper limits, using the formulae
#' \deqn{f_T(x) = f_X(x) / (F_X(upper) - F_X(lower))}
#' \deqn{F_T(x) = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
#' where \eqn{f_T}/\eqn{F_T} is the pdf/cdf of the truncated distribution T = Truncate(X, lower, upper) and
#' \eqn{f_X}, \eqn{F_X} is the pdf/cdf of the original distribution.
#'
#' @export
TruncatedDistribution <- R6Class("TruncatedDistribution", inherit = DistributionWrapper,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @examples
    #' TruncatedDistribution$new(
    #'   Binomial$new(prob = 0.5, size = 10),
    #'   lower = 2, upper = 4
    #' )
    #'
    #' # alternate constructor
    #' truncate(Binomial$new(), lower = 2, upper = 4)
    initialize = function(distribution, lower = NULL, upper = NULL) {

      assertDistribution(distribution)

      if (!isCdf(distribution) | !isPdf(distribution)) {
        stop("pdf and cdf is required for truncation. Try decorate(distribution, FunctionImputation) first.")
      }

      if (is.null(lower)) {
        lower <- distribution$inf
      } else if (lower < distribution$inf) {
        lower <- distribution$inf
      }
      if (is.null(upper)) {
        upper <- distribution$sup
      } else if (upper > distribution$sup) {
        upper <- distribution$sup
      }

      distlist <- list(distribution)
      names(distlist) <- distribution$short_name

      private$.outerParameters <- ParameterSet$new(
        id = list("lower", "upper"), value = list(lower, upper),
        support = list(Reals$new() + Set$new(-Inf, Inf), Reals$new() + Set$new(-Inf, Inf)),
        settable = list(TRUE, TRUE),
        description = list(
          "Lower limit of truncation.",
          "Upper limit of truncation."
        )
      )

      if (testDiscrete(distribution)) {
        support <- Interval$new(lower, upper, class = "integer")
      } else {
        support <- Interval$new(lower, upper)
      }

      super$initialize(
        distlist = distlist,
        name = paste("Truncated", distribution$name),
        short_name = paste0("Trunc", distribution$short_name),
        description = paste0(distribution$description, " Truncated between ", lower, " and ", upper, "."),
        support = support,
        type = distribution$traits$type,
        valueSupport = distribution$traits$valueSupport, variateForm = "univariate",
        outerID = "trunc"
      )
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }

      if ("trunc_lower" %in% names(lst) & "trunc_upper" %in% names(lst)) {
        checkmate::assert(lst[["trunc_lower"]] < lst[["trunc_upper"]], .var.name = "trunc_lower must be < trunc_upper")
      } else if ("trunc_lower" %in% names(lst)) {
        checkmate::assert(lst[["trunc_lower"]] < self$getParameterValue("trunc_upper"), .var.name = "trunc_lower must be < trunc_upper")
      } else if ("trunc_upper" %in% names(lst)) {
        checkmate::assert(lst[["trunc_upper"]] > self$getParameterValue("trunc_lower"), .var.name = "trunc_upper must be > trunc_lower")
      }

      super$setParameterValue(lst = lst, error = error)

      if (self$properties$support$class == "integer") {
        private$.properties$support <- Interval$new(self$getParameterValue("trunc_lower"), self$getParameterValue("trunc_upper"), class = "integer")
      } else {
        private$.properties$support <- Interval$new(self$getParameterValue("trunc_lower"), self$getParameterValue("trunc_upper"))
      }

      invisible(self)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower = self$getParameterValue("trunc_lower")
      upper = self$getParameterValue("trunc_upper")

      if (log) {
        pdf <- dist$pdf(x, log = TRUE) -
          log((dist$cdf(upper) - dist$cdf(lower)))
      } else {
        pdf <- dist$pdf(x) /
          (dist$cdf(upper) - dist$cdf(lower))
      }
      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      dist = self$wrappedModels()[[1]]
      lower = dist$cdf(self$getParameterValue("trunc_lower"))
      upper = dist$cdf(self$getParameterValue("trunc_upper"))
      cdfx = dist$cdf(x)

      if (lower.tail) {
        if (log.p) {
          cdf = log(cdfx - lower) - log(upper - lower)
        } else {
          cdf = (cdfx - lower)/(upper - lower)
        }
      } else {
        if (log.p) {
          cdf = log(upper - cdfx) - log(upper - lower)
        } else {
          cdf = (upper - cdfx)/(upper - lower)
        }
      }

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      dist = self$wrappedModels()[[1]]
      lower = dist$cdf(self$getParameterValue("trunc_lower"))
      upper = dist$cdf(self$getParameterValue("trunc_upper"))

      dist$quantile(p*(upper - lower) + lower, log.p = log.p, lower.tail = lower.tail)
    },
    .rand = function(n) {
      self$quantile(runif(n))
    }
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(TruncatedDistribution = TruncatedDistribution))

#' @title Truncate a Distribution
#' @description S3 functionality to truncate an R6 distribution.
#'
#' @param x Distribution.
#' @param lower lower limit for truncation.
#' @param upper upper limit for truncation.
#'
#' @seealso [TruncatedDistribution]
#'
#' @export
truncate <- function(x, lower = NULL, upper = NULL) {
  UseMethod("truncate", x)
}
#' @export
truncate.Distribution <- function(x, lower = NULL, upper = NULL) {
  TruncatedDistribution$new(x, lower, upper)
}
