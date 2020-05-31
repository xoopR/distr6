#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of multiple independent probability distributions.
#' @template class_vecdist
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#' @template param_decorators
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
#' @export
ProductDistribution <- R6Class("ProductDistribution",
  inherit = VectorDistribution,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @examples
    #' ProductDistribution$new(list(Binomial$new(
    #'   prob = 0.5,
    #'   size = 10
    #' ), Normal$new(mean = 15)))
    #'
    #' ProductDistribution$new(
    #'   distribution = "Binomial",
    #'   params = list(
    #'     list(prob = 0.1, size = 2),
    #'     list(prob = 0.6, size = 4),
    #'     list(prob = 0.2, size = 6)
    #'   )
    #' )
    #'
    #' # Equivalently
    #' ProductDistribution$new(
    #'   distribution = "Binomial",
    #'   params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))
    #' )
    initialize = function(distlist = NULL, distribution = NULL, params = NULL,
                          shared_params = NULL,
                          name = NULL, short_name = NULL,
                          decorators = NULL) {

      super$initialize(
        distlist = distlist,
        distribution = distribution,
        params = params,
        shared_params = shared_params,
        decorators = decorators,
        name = name,
        short_name = short_name
      )

      if (!is.null(name)) self$name <- gsub("Vector", "Product", self$name)
      if (!is.null(short_name)) self$short_name <- gsub("Vec", "X", self$short_name)
      self$description <- gsub("Vector", "Product", self$description)

      invisible(self)
    },

    #' @description
    #' Printable string representation of the `ProductDistribution`. Primarily used internally.
    #' @param n `(integer(1))`\cr
    #' Number of distributions to include when printing.
    strprint = function(n = 10) {
      str <- super$strprint(n = n)
      paste0(str, collapse = " X ")
    },

    #' @description
    #' Probability density function of the product distribution. Computed by
    #'  \deqn{f_P(X1 = x1,...,XN = xN) = \prod_{i} f_{Xi}(xi)}
    #'  where \eqn{f_{Xi}} are the pdfs of the wrapped distributions.
    #'@param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #' @examples
    #' p <- ProductDistribution$new(list(
    #' Binomial$new(prob = 0.5, size = 10),
    #' Binomial$new()))
    #' p$pdf(1:5)
    #' p$pdf(1, 2)
    #' p$pdf(1:2)
    pdf = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      product_dpqr_returner(
        dpqr = super$pdf(..., log = log, data = data),
        univariate = private$.univariate
      )
    },

    #' @description
    #' Cumulative distribution function of the product distribution. Computed by
    #'  \deqn{F_P(X1 = x1,...,XN = xN) = \prod_{i} F_{Xi}(xi)}
    #'  where \eqn{F_{Xi}} are the cdfs of the wrapped distributions.
    #'@param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #' @examples
    #' p <- ProductDistribution$new(list(
    #' Binomial$new(prob = 0.5, size = 10),
    #' Binomial$new()))
    #' p$cdf(1:5)
    #' p$cdf(1, 2)
    #' p$cdf(1:2)
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {
      product_dpqr_returner(
        dpqr = super$cdf(..., lower.tail = lower.tail, log.p = log.p, data = data),
        univariate = private$.univariate
      )
    },

    #' @description
    #' The quantile function is not implemented for product distributions.
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {
      stop("Quantile is currently unavailable for product distributions.")
    }
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(ProductDistribution = ProductDistribution))

#' @rdname ProductDistribution
#' @param x,y [Distribution]
#' @examples
#' Normal$new() * Binomial$new()
#' @export
`*.Distribution` <- function(x, y) {
  ProductDistribution$new(list(x, y))
}
