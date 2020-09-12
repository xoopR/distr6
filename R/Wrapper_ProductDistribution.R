#' @title Product Distribution Wrapper
#' @description A wrapper for creating the product distribution of multiple independent probability
#' distributions.
#'
#' @template class_vecdist
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#' @template param_decorators
#'
#' @details A product distribution is defined by
#'
#' \deqn{F_P(X1 = x1,...,XN = xN) = F_{X1}(x1) * ... * F_{XN}(xn)}{F_P(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)} #nolint
#' where \eqn{F_P} is the cdf of the product distribution and \eqn{X1,...,XN} are
#' independent distributions.
#'
#' @export
ProductDistribution <- R6Class("ProductDistribution",
  inherit = VectorDistribution,
  lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @examples
    #' \dontrun{
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
    #' }
    initialize = function(distlist = NULL, distribution = NULL, params = NULL,
                          shared_params = NULL,
                          name = NULL, short_name = NULL,
                          decorators = NULL,
                          vecdist = NULL) {

      if (!is.null(vecdist)) {
        checkmate::assertClass(vecdist, "VectorDistribution")

        if (!is.null(decorators)) {
          suppressMessages(decorate(self, decorators))
        }

        private$.modelTable <- vecdist$modelTable
        private$.distlist <- vecdist$distlist
        private$.univariate <- vecdist$.__enclos_env__$private$.univariate
        private$.pdf <- vecdist$.__enclos_env__$private$.pdf
        private$.cdf <- vecdist$.__enclos_env__$private$.cdf
        private$.quantile <- vecdist$.__enclos_env__$private$.quantile
        private$.rand <- vecdist$.__enclos_env__$private$.rand

        parameters  <- vecdist$parameters()

        if (checkmate::testClass(vecdist, "MixtureDistribution")) {
          parameters$.__enclos_env__$private$.parametersets$mix <- NULL
        }

        super$.__enclos_env__$super$initialize(
          distlist = if (vecdist$distlist) vecdist$wrappedModels() else NULL,
          name = vecdist$name,
          short_name = vecdist$short_name,
          description = vecdist$description,
          support = vecdist$properties$support,
          type = vecdist$traits$type,
          valueSupport = vecdist$traits$valueSupport,
          variateForm = "multivariate",
          parameters = parameters
        )

      } else {
        super$initialize(
          distlist = distlist,
          distribution = distribution,
          params = params,
          shared_params = shared_params,
          decorators = decorators,
          name = name,
          short_name = short_name
        )
      }

      if (is.null(name)) self$name <- gsub("Vector|Mixture", "Product", self$name)
      if (is.null(short_name)) self$short_name <- gsub("Vec|Mix", "Prod", self$short_name)
      self$description <- gsub("Vector|Mixture", "Product", self$description)

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
    #' @param ... `(numeric())` \cr
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
    #' @param ... `(numeric())` \cr
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

#' @title Coercion to Product Distribution
#' @description Helper functions to quickly convert compatible objects to
#' a [ProductDistribution].
#' @param object [MixtureDistribution] or [VectorDistribution]
#' @export
as.ProductDistribution <- function(object) {
  if (checkmate::testClass(object, "VectorDistribution")) {
    return(ProductDistribution$new(vecdist = object))
  } else {
    stop("Object must inherit from VectorDistribution.")
  }
}
