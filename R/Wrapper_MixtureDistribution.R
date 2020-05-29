#' @name MixtureDistribution
#' @title Mixture Distribution Wrapper
#' @description Wrapper used to construct a mixture of two or more distributions.
#'
#' @template method_pdf
#' @template method_cdf
#' @template method_quantile
#' @template method_rand
#' @template param_decorators
#' @template class_vecdist
#'
#' @return Returns an R6 object of class MixtureDistribution.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @examples
#' mixture$pdf(1)
#' mixture$cdf(1)
#' @export
MixtureDistribution <- R6Class("MixtureDistribution",
  inherit = VectorDistribution,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param weights `(character(1)|numeric())`\cr
    #' Weights to use in the resulting mixture. If all distributions are weighted equally then
    #' `"uniform"` provides a much faster implementation, otherwise a vector of length equal
    #' to the number of wrapped distributions, this is automatically scaled internally.
    #' @examples
    #' MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
    #'   weights = c(0.2, 0.8)
    #' )
    initialize = function(distlist = NULL, weights = "uniform", distribution = NULL, params = NULL,
                          shared_params = NULL,
                          name = NULL, short_name = NULL,
                          decorators = NULL) {

      lng <- if (is.null(distlist)) length(params) else length(distlist)

      if (checkmate::testNumeric(weights)) {
        stopifnot(length(weights) == lng)
        weights <- list(weights / sum(weights))
      } else if (weights != "uniform") {
        stop(sprintf("weights should either be a numeric of length %s, or 'uniform'", lng))
      }

      private$.outerParameters <- ParameterSet$new(
        id = "weights",
        value = weights,
        support = Interval$new(0, 1)^lng + Set$new("uniform"),
        settable = TRUE,
        description = "Mixture weights."
      )

      super$initialize(
        distlist = distlist,
        distribution = distribution,
        params = params,
        shared_params = shared_params,
        decorators = decorators,
        outerID = "mix",
        name = name,
        short_name = short_name
      )

      if (!is.null(name)) self$name <- gsub("Vector", "Mixture", self$name)
      if (!is.null(short_name)) self$short_name <- gsub("Vec", "Mix", self$short_name)
      self$description <- gsub("Vector", "Mixture", self$description)

      invisible(self)
    },

    #' @description
    #' Printable string representation of the `MixtureDistribution`. Primarily used internally.
    #' @param n `(integer(1))`\cr
    #' Number of distributions to include when printing.
    strprint = function(n = 10) {
      str <- super$strprint(n = n)
      paste0(str, collapse = " wX ")
    },

    #' @description
    #' Probability density function of the mixture distribution. Computed by
    #'  \deqn{f_M(x) = \sum_i (f_i)(x)*w_i}
    #'  where \eqn{w_i} is the vector of weights and \eqn{f_i} are the pdfs of the wrapped
    #'  distributions.
    #'
    #' Note that as this class inherits from [VectorDistribution], it is possible to evaluate
    #' the distributions at different points, but that this is not the usual use-case for
    #' mixture distributions.
    #'
    #' @examples
    #' m <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
    #'   weights = c(0.2, 0.8)
    #' )
    #' m$pdf(1:5)
    #' m$pdf(1)
    #' # also possible but unlikely to be used
    #' m$pdf(1, 2)
    pdf = function(..., log = FALSE, data = NULL) {
      mixture_dpqr_returner(
        dpqr = super$pdf(..., log = log, data = data),
        weights = private$.outerParameters$getParameterValue("weights"),
        univariate = private$.univariate
      )
    },

    #' @description
    #' Cumulative distribution function of the mixture distribution. Computed by
    #'  \deqn{F_M(x) = \sum_i (F_i)(x)*w_i}
    #'  where \eqn{w_i} is the vector of weights and \eqn{F_i} are the cdfs of the wrapped
    #'  distributions.
    #'
    #'  @examples
    #'  m <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
    #'   weights = c(0.2, 0.8)
    #' )
    #' m$cdf(1:5)
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, data = NULL) {
      mixture_dpqr_returner(
        dpqr = super$cdf(..., lower.tail = lower.tail, log.p = log.p, data = data),
        weights = private$.outerParameters$getParameterValue("weights"),
        univariate = private$.univariate
      )
    },

    #' @description
    #' The quantile function is not implemented for mixture distributions.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, data = NULL) {
      stop("Quantile is currently unavailable for mixture distributions.")
    },

    #' @description
    #' Simulation function for mixture distributions. Samples are drawn from a mixture by first
    #' sampling Multinomial(probs = weights, size = n), then sampling each distribution according
    #' to the samples from the Multinomial, and finally randomly permuting these draws.
    rand = function(n) {
      weights <- private$.outerParameters$getParameterValue("weights")

      lng <- nrow(self$modelTable)
      if (checkmate::testCharacter(weights)) {
        weights <- rep(1 / lng, lng)
      }

      x <- Multinomial$new(
        probs = weights,
        size = n
      )$rand(1)

      if (private$.univariate) {
        y <- c()
        for (i in seq(lng)) {
          y <- c(y, self[i]$rand(x[[i]]))
        }
      } else {
        y <- data.frame()
        for (i in seq(lng)) {
          y <- rbind(y, self[i]$rand(x[[i]]))
        }
      }

      if (length(y) == 1) {
        return(y)
      } else {
        if (inherits(y, "data.frame")) {
          return(apply(y, 2, sample, size = n))
        } else {
          return(sample(y, n))
        }
      }
    }
  ),

  private = list(
    .weights = numeric(0)
  )
)

.distr6$wrappers <- append(.distr6$wrappers, list(MixtureDistribution = MixtureDistribution))
