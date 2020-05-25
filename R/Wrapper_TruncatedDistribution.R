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

      if (!distribution$.__enclos_env__$private$.isCdf | !distribution$.__enclos_env__$private$.isPdf) {
        stop("pdf and cdf is required for truncation. Try decorate(Distribution, FunctionImputation) first.")
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

      pdf <- function(x1, ...) {
        self$wrappedModels()[[1]]$pdf(x1) / (self$wrappedModels()[[1]]$cdf(self$sup) - self$wrappedModels()[[1]]$cdf(self$inf))
      }
      formals(pdf)$self <- self

      cdf <- function(x1, ...) {
        num <- self$wrappedModels()[[1]]$cdf(x1) - self$wrappedModels()[[1]]$cdf(self$inf)
        den <- self$wrappedModels()[[1]]$cdf(self$sup) - self$wrappedModels()[[1]]$cdf(self$inf)
        return(num / den)
      }
      formals(cdf)$self <- self

      name <- paste("Truncated", distribution$name)
      short_name <- paste0("Trunc", distribution$short_name)

      distlist <- list(distribution)
      names(distlist) <- distribution$short_name

      description <- paste0(distribution$description, " Truncated between ", lower, " and ", upper, ".")

      private$.outerParameters <- ParameterSet$new(
        id = list("truncLower", "truncUpper"), value = list(lower, upper),
        support = list(Reals$new() + Set$new(-Inf, Inf), Reals$new() + Set$new(-Inf, Inf)),
        settable = list(FALSE, FALSE),
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
        distlist = distlist, pdf = pdf, cdf = cdf,
        name = name, short_name = short_name, support = support,
        type = distribution$type,
        description = description,
        valueSupport = distribution$valueSupport, variateForm = "univariate"
      )
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }

      if ("truncLower" %in% names(lst) & "truncUpper" %in% names(lst)) {
        checkmate::assert(lst[["truncLower"]] < lst[["truncUpper"]], .var.name = "truncLower must be < truncUpper")
      } else if ("truncLower" %in% names(lst)) {
        checkmate::assert(lst[["truncLower"]] < self$getParameterValue("truncUpper"), .var.name = "truncLower must be < truncUpper")
      } else if ("truncUpper" %in% names(lst)) {
        checkmate::assert(lst[["truncUpper"]] > self$getParameterValue("truncLower"), .var.name = "truncUpper must be > truncLower")
      }


      super$setParameterValue(lst = lst, error = error)
      if (self$support$class == "integer") {
        private$.properties$support <- Interval$new(self$getParameterValue("truncLower"), self$getParameterValue("truncUpper"), class = "integer")
      } else {
        private$.properties$support <- Interval$new(self$getParameterValue("truncLower"), self$getParameterValue("truncUpper"))
      }

      invisible(self)
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
