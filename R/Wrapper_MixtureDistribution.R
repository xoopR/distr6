#' @name MixtureDistribution
#' @title Mixture Distribution Wrapper
#' @description Wrapper used to construct a mixture of two or more distributions.
#'
#' @section Constructor: MixtureDistribution$new(distlist, weights = NULL, vectordist = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{weights} \tab numeric \tab Vector of weights. See Details. \cr
#' \code{vectordist} \tab numeric \tab Vector Distribution. See Details. \cr
#' }
#'
#' @details A Mixture Distribution is a weighted combination of two or more distributions such that for
#' pdf/cdfs of n distribution \eqn{f_1,...,f_n}/\eqn{F_1,...,F_n} and a given weight associated to each distribution,
#' \eqn{w_1,...,w_n}. The pdf of the mixture distribution \eqn{M(X1,...,XN)}, \eqn{f_M} is given by
#' \deqn{f_M = \sum_i (f_i)(w_i)}
#' and the cdf, F_M is given by
#' \deqn{F_M = \sum_i (F_i)(w_i)}
#'
#' If weights are given, they should be provided as a vector of numerics. If they don't sum to one
#' then they are normalised automatically. If NULL, they are taken to be uniform, i.e. for n
#' distributions, \eqn{w_i = 1/n, \ \forall \ i \ \in \ [1,n]}{w_i = 1/n, for all i \epsilon [1,n]}.
#'
#' Can optionally be constructed using a \code{VectorDistribution}, in which case \code{distlist} is ignored
#' and the mixture is constructed with the wrapped models in the vector.
#'
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @return Returns an R6 object of class MixtureDistribution.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @examples
#' mixture <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
#'   weights = c(0.2, 0.8)
#' )
#' mixture$pdf(1)
#' mixture$cdf(1)
#' @export
NULL
MixtureDistribution <- R6Class("MixtureDistribution", inherit = VectorDistribution,
  lock_objects = FALSE,
  public = list(
    initialize = function(distlist = NULL, weights = "uniform", distribution = NULL, params = NULL,
                          shared_params = NULL,
                          name = NULL, short_name = NULL,
                          decorators = NULL) {

      super$initialize(
        distlist = distlist,
        distribution = distribution,
        params = params,
        shared_params = shared_params,
        decorators = decorators
      )

      if (checkmate::testNumeric(weights)) {
        stopifnot(length(weights) == self$length)
        weights <- list(weights / sum(weights))
      } else if (weights != "uniform") {
        stop (sprintf("weights should either be a numeric of length %s, or 'uniform'", self$length))
      }

      private$.outerParameters <- ParameterSet$new(id = "weights",
                                                   value = weights,
                                                   support = Interval$new(0,1)^self$length + Set$new("uniform"),
                                                   settable = TRUE,
                                                   description = "Mixture weights.")

      self$name = gsub("Vector", "Mixture", self$name)
      self$short_name = gsub("Vec", "Mix", self$short_name)

      invisible(self)

      # TODO
      # if (is.null(description)) {
      #   description <- paste0(
      #     "Mixture of: ", paste0(1:length(distlist), ") ", lapply(distlist, function(x) x$description),
      #                            collapse = " And "
      #     ), " - With weights: (",
      #     paste0(weights, collapse = ", "), ")"
      #   )
      # }

      #self$description = description #TODO
      # private$.properties$support = setpower(Reals$new(), ndist)   # FIXME
      # private$.traits$type = setpower(Reals$new(), ndist)   # FIXME
    },

    pdf = function(..., log = FALSE, data) {
      mixture_dpqr_returner(dpqr = super$pdf(..., log = log, data = data),
                            weights = private$.outerParameters$getParameterValue("weights"),
                            univariate = private$.univariate)
    },

    cdf = function(..., lower.tail = TRUE, log.p = FALSE, data) {
      mixture_dpqr_returner(dpqr = super$cdf(..., lower.tail = lower.tail, log.p = log.p, data = data),
                            weights = private$.outerParameters$getParameterValue("weights"),
                            univariate = private$.univariate)
    },

    quantile = function(..., lower.tail = TRUE, log.p = FALSE, data) {
      stop("Quantile is currently unavailable for mixture distributions.")
    },

    rand = function(n){
      weights = private$.outerParameters$getParameterValue("weights")

      if (checkmate::testCharacter(weights)) {
        weights = rep(1/self$length, self$length)
      }

      x = Multinomial$new(probs = weights,
                          size = n)$rand(1)

      if (private$.univariate) {
        y = c()
        for (i in seq(self$length)) {
          y = c(y, self[i]$rand(x[[i]]))
        }
      } else {
        y = data.frame()
        for (i in seq(self$length)) {
          y = rbind(y, self[i]$rand(x[[i]]))
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

