#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of multiple independent probability distributions.
#' @template class_vecdist
#' @template method_pdf
#' @template method_cdf
#' @template method_quantile
#'
#' @details Exploits the following relationships of independent distributions
#'
#' \deqn{F_P(X1 = x1,...,XN = xN) = F_{X1}(x1) * ... * F_{XN}(xn)}{F_P(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)}
#' where \eqn{f_P}/\eqn{F_P} is the pdf/cdf of the joint (product) distribution \eqn{P} and \eqn{X1,...,XN} are independent distributions.
#'
#'
#' @section Constructor Details: A product distribution can either be constructed by a list of
#' distributions passed to \code{distlist} or by passing the name of a distribution implemented in distr6
#' to \code{distribution}, as well as a list or table of parameters to \code{params}. The former case provides more flexibility
#' in the ability to use multiple distributions but the latter is useful for quickly combining many
#' distributions of the same type. See examples.
#'
#' @return Returns an R6 object of class ProductDistribution.
#'
#' @examples
#' prodBin <- ProductDistribution$new(list(Binomial$new(
#'   prob = 0.5,
#'   size = 10
#' ), Normal$new(mean = 15)))
#' prodBin$pdf(x1 = 2, x2 = 3)
#' prodBin$cdf(1:5, 12:16)
#' prodBin$quantile(c(0.1, 0.2), c(0.3, 0.4))
#' prodBin$rand(10)
#'
#' prodBin <- ProductDistribution$new(
#'   distribution = Binomial,
#'   params = list(
#'     list(prob = 0.1, size = 2),
#'     list(prob = 0.6, size = 4),
#'     list(prob = 0.2, size = 6)
#'   )
#' )
#' prodBin$pdf(x1 = 1, x2 = 2, x3 = 3)
#' prodBin$cdf(x1 = 1, x2 = 2, x3 = 3)
#' prodBin$rand(10)
#'
#' # Equivalently
#' prodBin <- ProductDistribution$new(
#'   distribution = "Binomial",
#'   params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))
#' )
#' prodBin$pdf(x1 = 1, x2 = 2, x3 = 3)
#' prodBin$cdf(x1 = 1, x2 = 2, x3 = 3)
#' prodBin$rand(10)
#' @export
ProductDistribution <- R6Class("ProductDistribution",
  inherit = VectorDistribution,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(distlist = NULL, distribution = NULL, params = NULL,
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

      self$name <- gsub("Vector", "Product", self$name)
      self$short_name <- gsub("Vec", "Prod", self$short_name)

      invisible(self)

      # TODO
      # type = do.call(setproduct, lapply(distlist,function(x) x$type))
      # support = do.call(setproduct, lapply(distlist,function(x) x$support))
      # if("discrete" %in% lapply(distlist, valueSupport))
      #   valueSupport = "discrete"
      # else
      #   valueSupport = "continuous"
    },

    #' @description
    #' Probability density function of the product distribution. Computed by
    #'  \deqn{f_P(X1 = x1,...,XN = xN) = \prod_{i} f_{Xi}(xi)}
    #'  where \eqn{f_{Xi}} are the pdfs of the wrapped distributions.
    #'
    #' @examples
    #' p <- ProductDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
    #'   weights = c(0.2, 0.8)
    #' )
    #' p$pdf(1:5)
    #' p$pdf(1)
    #' p$pdf(1, 2)
    pdf = function(..., log = FALSE, data = NULL) {
      product_dpqr_returner(
        dpqr = super$pdf(..., log = log, data = data),
        univariate = private$.univariate
      )
    },

    #' @description
    #' Cumulative distribution function of the product distribution. Computed by
    #'  \deqn{F_P(X1 = x1,...,XN = xN) = \prod_{i} F_{Xi}(xi)}
    #'  where \eqn{F_{Xi}} are the cdfs of the wrapped distributions.
    #'
    #' @examples
    #' p <- ProductDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
    #'   weights = c(0.2, 0.8)
    #' )
    #' p$cdf(1:5)
    #' p$cdf(1)
    #' p$cdf(1, 2)
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, data = NULL) {
      product_dpqr_returner(
        dpqr = super$cdf(..., lower.tail = lower.tail, log.p = log.p, data = data),
        univariate = private$.univariate
      )
    },

    #' @description
    #' The quantile function is not implemented for product distributions.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, data = NULL) {
      stop("Quantile is currently unavailable for product distributions.")
    }
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(ProductDistribution = ProductDistribution))
