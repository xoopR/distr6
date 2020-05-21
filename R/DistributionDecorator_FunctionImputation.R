#' @name FunctionImputation
#'
#' @title Imputed Pdf/Cdf/Quantile/Rand Functions
#'
#' @description This decorator imputes missing pdf/cdf/quantile/rand methods from R6 Distributions
#' by using strategies dependent on which methods are already present in the distribution.
#'
#' @details Decorator objects add functionality to the given Distribution object by copying methods
#' in the decorator environment to the chosen Distribution environment. See the 'Added Methods' section
#' below to find details of the methods that are added to the Distribution. Methods already
#' present in the distribution are not overwritten by the decorator.
#'
#' Use \code{\link{decorate}} to decorate a Distribution.
#'
#' All methods in this decorator use numerical approximations and therefore better results may be available
#' from analytic computations.
#'
#' @section Constructor: FunctionImputation$new(distribution)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab Density/mass function \tab \code{\link{pdf}} \cr
#' \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab Distribution function \tab \code{\link{cdf}}\cr
#' \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab Quantile function \tab \code{\link{quantile.Distribution}} \cr
#' \code{rand(n, simplify = TRUE)} \tab Simulation function \tab \code{\link{rand}} \cr
#' }
#'
#' @seealso \code{\link{decorate}}, \code{\link{listDecorators}}
#'
#' @return Returns a decorated R6 object inheriting from class SDistribution with d/p/q/r numerically
#' imputed if previously missing.
#'
#' @examples
#' x <- Distribution$new("Test",
#'   pdf = function(x) 1 / (4 - 1),
#'   support = set6::Interval$new(1, 4),
#'   type = set6::Reals$new()
#' )
#' decorate(x, FunctionImputation)
#' x$pdf(0:5)
#' x$cdf(0:5)
#' @export
NULL

FunctionImputation <- R6Class("FunctionImputation", inherit = DistributionDecorator)
.distr6$decorators <- append(.distr6$decorators, list(FunctionImputation = FunctionImputation))
FunctionImputation$set("public", "packages", c("pracma", "GoFKernel"))

FunctionImputation$set("public", "pdf", function(x1) {
  # CDF2PDF
  if (testUnivariate(self)) {
    if (testDiscrete(self)) {
      return(self$cdf(x1) - self$cdf(x1 - 1))
    } else if (testContinuous(self)) {
      message(.distr6$message_numeric)
      return(pracma::fderiv(self$cdf, x1))
    }
  }
})
FunctionImputation$set("public", "cdf", function(x, lower.tail = TRUE, log.p = FALSE, n = 10001,
                                                 .cdf_x = NULL) {

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
})
FunctionImputation$set("public", "quantile", function(p, lower.tail = TRUE, log.p = FALSE,
                                                      n = 10001) {

  message(.distr6$message_numeric)

  x <- impute_genx(self, n)

  if (private$.isCdf) {
    return(NumericQuantile(p, x, self$cdf(x), lower.tail, log.p))
  } else {
    return(NumericQuantile(p, x, self$cdf(x, n = cdf_n, .cdf_x = x), lower.tail, log.p))
  }

})

FunctionImputation$set("public", "rand", function(n) {

  if (private$.isQuantile) {
    return(self$quantile(runif(n)))
  } else {
    x <- impute_genx(self, n)
    return(sample(x, n, TRUE, self$pdf(x)))
  }

})
