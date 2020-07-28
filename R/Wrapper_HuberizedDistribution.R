#' @name HuberizedDistribution
#' @title Distribution Huberization Wrapper
#' @description A wrapper for huberizing any probability distribution at given limits.
#' @template class_wrapper
#' @template class_trunchub
#' @template method_setParameterValue
#'
#' @details
#' Huberizes a distribution at lower and upper limits, using the formula
#' \deqn{f_H(x) = F(x), if x \le lower}
#' \deqn{f_H(x) = f(x), if lower < x < upper}
#' \deqn{f_H(x) = F(x), if x \ge upper}
#' where f_H is the pdf of the truncated distribution H = Huberize(X, lower, upper) and
#' \eqn{f_X}/\eqn{F_X} is the pdf/cdf of the original distribution.
#'
#' @export
HuberizedDistribution <- R6Class("HuberizedDistribution",
  inherit = DistributionWrapper,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @examples
    #' HuberizedDistribution$new(
    #'   Binomial$new(prob = 0.5, size = 10),
    #'   lower = 2, upper = 4
    #' )
    #'
    #' # alternate constructor
    #' huberize(Binomial$new(), lower = 2, upper = 4)
    initialize = function(distribution, lower = NULL, upper = NULL) {

      assertDistribution(distribution)

      if (testMultivariate(distribution)) {
        stop("Huberization not currently available for multivariate distributions.")
      }

      if (testMixture(distribution)) {
        stop("Huberization not currently available for mixed distributions.")
      }

      if (isPdf(distribution) == 0 | isCdf(distribution) == 0) {
        stop("pdf and cdf are required for huberization.
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

      if (testDiscrete(distribution)) {
        support <- Interval$new(lower, upper, class = "integer")
      } else if (testContinuous(distribution)) {
        support <- Interval$new(lower, upper)
      }

      private$.outerParameters <- ParameterSet$new(
        id = list("lower", "upper"), value = list(lower, upper),
        support = list(Reals$new() + Set$new(-Inf, Inf), Reals$new() + Set$new(-Inf, Inf)),
        description = list(
          "Lower limit of huberization",
          "Upper limit of huberization"
        )
      )
      private$.outerParameters$addChecks(function(self) self$getParameterValue("lower") <
                                           self$getParameterValue("upper"))

      super$initialize(
        distlist = distlist,
        name = paste("Huberized", distribution$name),
        short_name = paste0("Hub", distribution$short_name),
        description = paste0(
          distribution$description, " Huberized between ", lower, " and ",
          upper, "."
        ),
        support = support,
        type = distribution$traits$type,
        valueSupport = "mixture", variateForm = "univariate",
        outerID = "hub"
      )
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$properties$support$class == "integer") {
        private$.properties$support <- Interval$new(self$getParameterValue("hub_lower"),
          self$getParameterValue("hub_upper"),
          class = "integer"
        )
      } else {
        private$.properties$support <- Interval$new(
          self$getParameterValue("hub_lower"),
          self$getParameterValue("hub_upper")
        )
      }

      invisible(self)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      dist <- self$wrappedModels()[[1]]

      if (testDiscrete(dist)) {
        lower <- self$getParameterValue("hub_lower")
        upper <- self$getParameterValue("hub_upper")

        pdf <- x
        pdf[x < lower | x > upper] <- 0
        pdf[x == lower] <- dist$cdf(lower)
        pdf[x > lower & x < upper] <- dist$pdf(x[x > lower & x < upper])
        pdf[x == upper] <- 1 - dist$cdf(upper) + dist$pdf(upper)

        return(pdf)
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower <- self$getParameterValue("hub_lower")
      upper <- self$getParameterValue("hub_upper")

      cdf <- x
      cdf[x < lower] <- 0
      cdf[x >= upper] <- 1
      cdf[x >= lower & x < upper] <- dist$cdf(cdf[x >= lower & x < upper])

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      dist <- self$wrappedModels()[[1]]
      lower <- self$getParameterValue("hub_lower")
      upper <- self$getParameterValue("hub_upper")

      quantile <- dist$quantile(p, lower.tail = lower.tail, log.p = log.p)

      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p
      quantile[p == 0] <- lower
      quantile[p == 1] <- upper
      quantile[quantile > upper] <- upper
      quantile[quantile < lower] <- lower

      return(quantile)
    },
    .rand = function(n) {
      self$quantile(runif(n))
    }
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(HuberizedDistribution = HuberizedDistribution))

#' @title Huberize a Distribution
#' @description S3 functionality to huberize an R6 distribution.
#'
#' @param x distribution to huberize.
#' @param lower lower limit for huberization.
#' @param upper upper limit for huberization.
#'
#' @seealso [HuberizedDistribution]
#'
#' @export
huberize <- function(x, lower, upper) {
  UseMethod("huberize", x)
}
#' @export
huberize.Distribution <- function(x, lower = NULL, upper = NULL) {
  HuberizedDistribution$new(x, lower, upper)
}
