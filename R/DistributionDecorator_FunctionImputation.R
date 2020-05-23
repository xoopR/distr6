#' @title Imputed Pdf/Cdf/Quantile/Rand Functions
#'
#' @description This decorator imputes missing pdf/cdf/quantile/rand methods from R6 Distributions
#' by using strategies dependent on which methods are already present in the distribution.
#'
#' @template class_decorator
#' @template field_packages
#' @template method_pdf
#' @template method_cdf
#' @template method_quantile
#' @template method_rand
#' @template method_decorate
#'
#' @examples
#' x <- Distribution$new("Test", pdf = function(x) 1 / (4 - 1),
#' support = set6::Interval$new(1, 4), type = set6::Reals$new())
#' decorate(x, FunctionImputation)
#'
#' x <- Distribution$new("Test", pdf = function(x) 1 / (4 - 1),
#' support = set6::Interval$new(1, 4), type = set6::Reals$new(),
#' decorators = ExoticStatistics)
#'
#' x <- Distribution$new("Test", pdf = function(x) 1 / (4 - 1),
#' support = set6::Interval$new(1, 4), type = set6::Reals$new())
#' ExoticStatistics$new()$decorate(x)
#'
#' @export
FunctionImputation <- R6Class("FunctionImputation", inherit = DistributionDecorator,
  public = list(
    packages = c("pracma", "GoFKernel"),

    #' @description
    #' Decorates the given distribution with the methods available in this decorator.
    decorate = function(distribution) {

      assert_pkgload(self$packages)

      if (!testUnivariate(distribution)) {
        stop("FunctionImputation is currently only supported for univariate distributions.")
      }
      pdist = distribution$.__enclos_env__$private
      if (!pdist$.isPdf) {
        pdf <- FunctionImputation$public_methods$pdf
        formals(pdf)$self <- distribution
        pdist$.pdf <- pdf
        pdist$.isPdf <- TRUE
      }
      if (!pdist$.isCdf) {
        cdf <- FunctionImputation$public_methods$cdf
        formals(cdf)$self <- distribution
        pdist$.cdf <- cdf
        pdist$.isCdf <- TRUE
      }
      if (!pdist$.isQuantile) {
        quant <- FunctionImputation$public_methods$quantile
        formals(quant)$self <- distribution
        pdist$.quantile <- quant
        pdist$.isQuantile <- TRUE
      }
      if (!pdist$.isRand) {
        rand <- FunctionImputation$public_methods$rand
        formals(rand)$self <- distribution
        pdist$.rand <- rand
        pdist$.isRand <- TRUE
      }

      invisible(self)
    },

    #' @description
    #' Numerical approximation to pdf. Imputed by subtracting or taking the numerical derivative
    #' of the cdf with [pracma::fderiv].
    pdf = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      # CDF2PDF
      if (testUnivariate(self)) {
        if (testDiscrete(self)) {
          return(self$cdf(..) - self$cdf(... - 1))
        } else if (testContinuous(self)) {
          message(.distr6$message_numeric)
          return(pracma::fderiv(self$cdf, ...))
        }
      }
    },

    #' @description
    #' Numerical approximation to cdf. Imputed by adding or taking the numerical integral
    #' of the pdf.
    #' @param n `(numeric(1))` \cr
    #' Number of points to use when imputing a continuous cdf from the `pdf`, Simpson's rule
    #' is used for imputation.
    #' @param .cdf_x `(numeric())` \cr
    #' Parameter used by internal methods, should be ignored.
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL,
                   n = 10001, .cdf_x = NULL) {

      message(.distr6$message_numeric)

      # PDF2CDF
      if (testUnivariate(self)) {

        data <- x
        # if (testDiscrete(self)) {
        if (!is.null(.cdf_x)) {
          x <- .cdf_x
        } else {
          x <- impute_genx(self, n)
        }


        if (testDiscrete(self)) {
          return(
            NumericCdf_Discrete(q = data,
                                x = x,
                                pdf = self$pdf(x),
                                lower = lower.tail,
                                logp = log.p)
          )
        } else {
          return(
            NumericCdf_Continuous(q = data,
                                  x = x,
                                  pdf = self$pdf(x),
                                  lower = lower.tail,
                                  logp = log.p)
          )
        }
      } else {
        stop("CDF imputation currently only implemented for univariate distributions.")
      }
    },

    #' @description
    #' Numerical approximation to quantile. Imputed by creating a grid of points over the cdf
    #' and finding the inverse-cdf at the closest point on the grid.
    #' @param n `(numeric(1))` \cr
    #' Number of points to use when imputing the quantile from the `cdf`. If an analytical `cdf`
    #' is not available then this is imputed first.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL,
                        n = 10001) {

      message(.distr6$message_numeric)

      x <- impute_genx(self, n)

      if (private$.isCdf) {
        return(NumericQuantile(p, x, self$cdf(x), lower.tail, log.p))
      } else {
        return(NumericQuantile(p, x, self$cdf(x, n = n, .cdf_x = x), lower.tail, log.p))
      }

    },
    #' @description
    #' Numerical approximation to simulation. If analytical quantile is available uses inverse-transform
    #' sampling. Otherwise if pdf is available then imputes by creating a fine grid of points in the
    #' distribution support and then sampling these. Otherwise uses inverse-transform sampling after
    #' first imputing the quantile function.
    #' @param size_n `(numeric(1))` \cr
    #' Determines size of grid of points to sample from if no analytical quantile is available.
    rand = function(n, simplify = TRUE, size_n = 10001) {
      if (private$.isQuantile) {
        return(self$quantile(runif(n)))
      } else if (private$.isPdf) {
        return(sample(x, n, TRUE, self$pdf(impute_genx(self, size_n))))
      } else {
        return(self$quantile(runif(n)), n = size_n)
      }
    }
  ))

.distr6$decorators <- append(.distr6$decorators, list(FunctionImputation = FunctionImputation))
