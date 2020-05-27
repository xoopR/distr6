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
#' decorate(x, "FunctionImputation")
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
      if (!isPdf(distribution)) {
        pdf <- FunctionImputation$public_methods$pdf
        formals(pdf)$self <- distribution
        unlockBinding("pdf", distribution)
        distribution$pdf <- pdf
        pdist$.isPdf <- -1L
        lockBinding("pdf", distribution)
        lockBinding(".isPdf", pdist)
      }
      if (!isCdf(distribution)) {
        cdf <- FunctionImputation$public_methods$cdf
        formals(cdf)$self <- distribution
        unlockBinding("cdf", distribution)
        distribution$cdf <- cdf
        pdist$.isCdf <- -1L
        lockBinding("cdf", distribution)
        lockBinding(".isCdf", pdist)
      }
      if (!isQuantile(distribution)) {
        quant <- FunctionImputation$public_methods$quantile
        formals(quant)$self <- distribution
        unlockBinding("quantile", distribution)
        distribution$quantile <- quant
        pdist$.isQuantile <- -1L
        lockBinding("quantile", distribution)
        lockBinding(".isQuantile", pdist)
      }
      if (!isRand(distribution)) {
        rand <- FunctionImputation$public_methods$rand
        formals(rand)$self <- distribution
        unlockBinding("rand", distribution)
        distribution$rand <- rand
        pdist$.isRand <- -1L
        lockBinding("rand", distribution)
        lockBinding(".isRand", pdist)
      }

      invisible(self)
    },

    #' @description
    #' Numerical approximation to pdf. Imputed by subtracting or taking the numerical derivative
    #' of the cdf with [pracma::fderiv]. For discrete distributions, assumes support
    #' has no breaks on the integers.
    pdf = function(..., log = FALSE, simplify = TRUE, data = NULL) {

        data <- pdq_point_assert(..., self = self, data = data)

        if (testDiscrete(self)) {
          data <- as.matrix(data)
          pdf <- self$cdf(data = data) - self$cdf(data = data - 1)
        } else if (testContinuous(self)) {
          message(.distr6$message_numeric)
          pdf <- pracma::fderiv(self$cdf, data)
        }

        if (log) {
          pdf = log(pdf)
        }

        pdqr_returner(pdf, simplify, self$short_name)
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
                   n = 1000, .cdf_x) {

      message(.distr6$message_numeric)

        data <- pdq_point_assert(..., self = self, data = data)

        if (testDiscrete(self)) {
          if (missing(.cdf_x)) {
            x <- impute_genx(self, n)
          } else {
            x <- .cdf_x
          }
          cdf <- C_NumericCdf_Discrete(q = data,
                                x = x,
                                pdf = self$pdf(x),
                                lower = lower.tail,
                                logp = log.p)
        } else {
          cdf <- numeric(length(data))
          for (i in seq_along(data)) {
            cdf[i] <- integrate(self$pdf, 0, data[i])$value
          }
        }

        pdqr_returner(cdf, simplify, self$short_name)
    },

    #' @description
    #' Numerical approximation to quantile. Imputed by creating a grid of points over the cdf
    #' and finding the inverse-cdf at the closest point on the grid.
    #' @param n `(numeric(1))` \cr
    #' Number of points to use when imputing the quantile from the `cdf`. If an analytical `cdf`
    #' is not available then this is imputed first.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL,
                        n = 1000) {

      message(.distr6$message_numeric)
      data <- pdq_point_assert(..., self = self, data = data)
      # x <- impute_genx(self, n)
      x <- self$workingSupport
      lower <- x$lower
      upper <- x$upper

      # if (isCdf(self)) {
      #
      if (testContinuous(self)) {
        quantile <- numeric(length(data))
        for (i in seq_along(data)) {
          quantile[i] <- suppressMessages(GoFKernel::inverse(self$cdf, lower = lower, upper = upper)(data[i]))
        }
      } else {
        x <- impute_genx(self, n)
        if (isCdf(self)) {
          quantile <- C_NumericQuantile(data, x, self$cdf(x), lower.tail, log.p)
        } else {
          quantile <- C_NumericQuantile(data, x, self$cdf(x, .cdf_x = x), lower.tail, log.p)
        }
      }

      pdqr_returner(quantile, simplify, self$short_name)
    },
    #' @description
    #' Numerical approximation to simulation. If analytical quantile is available uses inverse-transform
    #' sampling. Otherwise if pdf is available then imputes by creating a fine grid of points in the
    #' distribution support and then sampling these. Otherwise uses inverse-transform sampling after
    #' first imputing the quantile function.
    #' @param size_n `(numeric(1))` \cr
    #' Determines size of grid of points to sample from if no analytical quantile is available.
    rand = function(n, simplify = TRUE, size_n = 10001) {
      message(.distr6$message_numeric)
      if (isQuantile(self) == 1L) {
        return(self$quantile(runif(n)))
      } else if (isPdf(self) == 1L) {
        x <- impute_genx(self, size_n)
        return(sample(x, n, TRUE, self$pdf(x)))
      } else {
        return(self$quantile(runif(n), n = size_n))
      }
    }
  ))

.distr6$decorators <- append(.distr6$decorators, list(FunctionImputation = FunctionImputation))
