#' @name TruncatedDistribution
#' @title Distribution Truncation Wrapper
#' @description A wrapper for truncating any probability distribution at given limits.
#'
#' @details Truncates a distribution at lower and upper limits, using the formulae
#' \deqn{f_T(x) = f_X(x) / (F_X(upper) - F_X(lower))}
#' \deqn{F_T(x) = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
#' where \eqn{f_T}/\eqn{F_T} is the pdf/cdf of the truncated distribution T = Truncate(X, lower, upper) and
#' \eqn{f_X}, \eqn{F_X} is the pdf/cdf of the original distribution.
#'
#' If lower or upper are NULL they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#' The support of the new distribution is the interval of points between lower and upper.
#'
#' The pdf and cdf of the distribution are required for this wrapper, if unavailable decorate with
#' \code{FunctionImputation} first.
#'
#' @section Constructor: TruncatedDistribution$new(distribution, lower = NULL, upper = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to truncate. \cr
#' \code{lower} \tab numeric \tab Lower limit for truncation. \cr
#' \code{upper} \tab numeric \tab Upper limit for truncation.
#' }
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}}, \code{\link{FunctionImputation}}, \code{\link{truncate}}
#'
#' @examples
#' truncBin <- TruncatedDistribution$new(
#'             Binomial$new(prob = 0.5, size = 10),
#'             lower = 2, upper = 4)
#' truncBin$getParameterValue("prob")
#'
#' @export
NULL
TruncatedDistribution <- R6::R6Class("TruncatedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
TruncatedDistribution$set("public","initialize",function(distribution, lower = NULL, upper = NULL){

  assertDistribution(distribution)

  if(is.null(distribution$cdf(1)) | is.null(distribution$pdf(1)))
    stop("pdf and cdf is required for truncation. Try decorate(Distribution, FunctionImputation) first.")

  if(is.null(lower))
    lower <- distribution$inf()
  else if(lower < distribution$inf())
    lower <- distribution$inf()
  if(is.null(upper))
    upper <- distribution$sup()
  else if(upper > distribution$sup())
    upper <- distribution$sup()

  pdf <- function(x1,...) {
    self$wrappedModels()[[1]]$pdf(x1) / (self$wrappedModels()[[1]]$cdf(self$sup()) - self$wrappedModels()[[1]]$cdf(self$inf()))
  }
  formals(pdf)$self <- self

  cdf <- function(x1,...){
    num = self$wrappedModels()[[1]]$cdf(x1) - self$wrappedModels()[[1]]$cdf(self$inf())
    den = self$wrappedModels()[[1]]$cdf(self$sup()) - self$wrappedModels()[[1]]$cdf(self$inf())
    return(num/den)
  }
  formals(cdf)$self <- self

  name = paste("Truncated",distribution$name)
  short_name = paste0("Truncated",distribution$short_name)

  distlist = list(distribution)
  names(distlist) = distribution$short_name

  description = paste0(distribution$description, " Truncated between ",lower," and ",upper,".")

  if(testDiscrete(distribution))
    support <- Set$new(lower:upper)
  else
    support <- Interval$new(lower,upper)

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf,
                   name = name, short_name = short_name, support = support,
                   type = distribution$type(),
                   description = description)
})

#' @title Truncate a Distribution
#' @description S3 functionality to truncate an R6 distribution.
#'
#' @param x Distribution.
#' @param lower lower limit for truncation.
#' @param upper upper limit for truncation.
#'
#' @seealso \code{\link{TruncatedDistribution}}
#'
#' @export
truncate <- function(x,lower = NULL,upper = NULL){
  UseMethod("truncate", x)
}
#' @export
truncate.Distribution <- function(x, lower = NULL, upper = NULL){
  TruncatedDistribution$new(x, lower, upper)
}
