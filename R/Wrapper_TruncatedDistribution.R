#' @name TruncatedDistribution
#' @title Distribution Truncation Wrapper
#' @description A wrapper for truncating any probability distribution at given limits.
#' @template class_wrapper
#' @template class_trunchub
#' @template method_setParameterValue
#'
#' @details
#' Truncates a distribution at lower and upper limits on a left-open interval, using the formulae
#' \deqn{f_T(x) = f_X(x) / (F_X(upper) - F_X(lower))}
#' \deqn{F_T(x) = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
#' where \eqn{f_T}/\eqn{F_T} is the pdf/cdf of the truncated distribution
#' T = Truncate(X, lower, upper) and \eqn{f_X}, \eqn{F_X} is the pdf/cdf of the
#' original distribution. T is supported on (].
#'
#' @export
TruncatedDistribution <- R6Class("TruncatedDistribution",
  inherit = DistributionWrapper,
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

      if (testMultivariate(distribution)) {
        stop("Truncation not currently available for multivariate distributions.")
      }

      if (testMixture(distribution)) {
        stop("Truncation not currently available for mixed distributions.")
      }

      if (isCdf(distribution) == 0 | isPdf(distribution) == 0) {
        stop("pdf and cdf is required for truncation.
Try decorate(distribution, FunctionImputation) first.")
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
        description = list(
          "Lower limit of truncation",
          "Upper limit of truncation"
        )
      )
      private$.outerParameters$addChecks(function(self) self$getParameterValue("lower") <
                                           self$getParameterValue("upper"))

      if (testDiscrete(distribution)) {
        support <- Interval$new(lower + 1, upper, class = "integer", type = "(]")
      } else {
        support <- Interval$new(lower, upper, type = "(]")
      }

      super$initialize(
        distlist = distlist,
        name = paste("Truncated", distribution$name),
        short_name = paste0("Trunc", distribution$short_name),
        description = paste0(
          distribution$description, " Truncated between ", lower, " and ",
          upper, "."
        ),
        support = support,
        type = distribution$traits$type,
        valueSupport = distribution$traits$valueSupport, variateForm = "univariate",
        outerID = "trunc"
      )
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)

      if (self$properties$support$class == "integer") {
        private$.properties$support <- Interval$new(self$getParameterValue("trunc_lower") + 1,
          self$getParameterValue("trunc_upper"),
          class = "integer"
        )
      } else {
        private$.properties$support <- Interval$new(self$getParameterValue("trunc_lower"),
          self$getParameterValue("trunc_upper"),
          type = "(]"
        )
      }

      invisible(self)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower <- self$getParameterValue("trunc_lower")
      upper <- self$getParameterValue("trunc_upper")

      if (log) {
        pdf <- rep(-Inf, length(x))
        pdf[x > lower & x <= upper] <- dist$pdf(x[x > lower & x <= upper], log = TRUE) -
          log((dist$cdf(upper) - dist$cdf(lower)))
      } else {
        pdf <- numeric(length(x))
        pdf[x > lower & x <= upper] <- dist$pdf(x[x > lower & x <= upper]) /
          (dist$cdf(upper) - dist$cdf(lower))
      }

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower <- self$getParameterValue("trunc_lower")
      upper <- self$getParameterValue("trunc_upper")
      Flower <- dist$cdf(lower)
      Fupper <- dist$cdf(upper)
      Fx <- dist$cdf(x)

      if (lower.tail) {
        if (log.p) {
          cdf <- log(Fx - Flower) - log(Fupper - Flower)
          cdf[x <= lower] <- -Inf
          cdf[x >= upper] <- 0
        } else {
          cdf <- (Fx - Flower) / (Fupper - Flower)
          cdf[x <= lower] <- 0
          cdf[x >= upper] <- 1
        }
      } else {
        if (log.p) {
          cdf <- log(Fupper - Fx) - log(Fupper - Flower)
          cdf[x <= lower] <- 0
          cdf[x >= upper] <- -Inf
        } else {
          cdf <- (Fupper - Fx) / (Fupper - Flower)
          cdf[x <= lower] <- 1
          cdf[x >= upper] <- 0
        }
      }

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower <- self$getParameterValue("trunc_lower")
      upper <- self$getParameterValue("trunc_upper")

      Fupper <- dist$cdf(upper)
      Flower <- dist$cdf(lower)

      quant <- numeric(length(p))
      p <- p * (Fupper - Flower) + Flower
      quant[p >= 1] <- upper
      quant[p <= 0] <- lower
      quant[p > 0 & p < 1] <- dist$quantile(p[p > 0 & p < 1], log.p = log.p,
                                          lower.tail = lower.tail)
      return(quant)
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
