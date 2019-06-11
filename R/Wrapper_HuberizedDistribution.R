#' @name HuberizedDistribution
#' @title Distribution Huberization Wrapper
#' @description A wrapper for huberizing any probability distribution at given limits.
#'
#' @section Constructor: HuberizedDistribution$new(distribution, lower = NULL, upper = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to huberize. \cr
#' \code{lower} \tab numeric \tab Lower limit for huberization. \cr
#' \code{upper} \tab numeric \tab Upper limit for huberization.
#' }
#'
#' @details Huberizes a distribution at lower and upper limits, using the formula
#' \tabular{lll}{
#'  \tab \eqn{F(x)} \tab if \eqn{x \le lower} \cr
#'  f_H(x) = \tab \eqn{f(x)} \tab if \eqn{lower < x < upper} \cr
#'  \tab \eqn{1 - F(x)} \tab if \eqn{x \ge upper} \cr
#' }
#' where f_H is the pdf of the truncated distribution H = Huberize(X, lower, upper) and f_X/F_X is the
#' pdf/cdf of the original distribution.
#'
#' If lower or upper are NULL they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#'
#' @section Public Methods:
#' See \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
#'
#' @seealso \code{\link{listWrappers}}.
#'
#' @examples
#' hubBin <- HuberizedDistribution$new(Binomial$new(prob = 0.5, size = 10), lower = 2, upper = 4)
#' hubBin$getParameterValue("prob")
#' hubBin$pdf(2)
NULL

#' @export
HuberizedDistribution <- R6::R6Class("HuberizedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
HuberizedDistribution$set("private", ".cutoffInterval", NULL)
HuberizedDistribution$set("public","initialize",function(distribution, lower = NULL, upper = NULL){

  assertDistribution(distribution)

  if(is.null(distribution$cdf(1)))
    stop("cdf is required for huberization. Try decorate(Distribution, FunctionImputation) first.")

  if(is.null(lower)) lower = distribution$inf()
  if(is.null(upper)) upper = distribution$sup()

  pdf <- function(x1, ...){
    if(x1 <= private$.cutoffInterval[[1]])
      return(self$wrappedModels()[[1]]$cdf(private$.cutoffInterval[[1]]))
    else if(x1 >= private$.cutoffInterval[[2]])
      return(1-self$wrappedModels()[[1]]$cdf(private$.cutoffInterval[[2]]))
    else
      return(self$wrappedModels()[[1]]$pdf(x1))
  }

  name = paste("Huberized",distribution$name)
  short_name = paste0("Huberized",distribution$short_name)

  distlist = list(distribution)
  names(distlist) = distribution$short_name

  private$.cutoffInterval = c(lower, upper)

  description = paste0(distribution$description, " Huberized between ",lower," and ",upper,".")

  super$initialize(distlist = distlist, pdf = pdf, name = name,
                   short_name = short_name, type = distribution$type(),
                   support = distribution$support(), distrDomain = distribution$distrDomain(),
                   prefixParams = FALSE, description = description)
}) # IN PROGRESS

#' @title Huberize a Distribution
#' @description S3 functionality to huberize an R6 distribution.
#'
#' @param x distribution to huberize.
#' @param lower lower limit for huberization.
#' @param upper upper limit for huberization.
#'
#' @seealso \code{\link{HuberizedDistribution}}
#'
#' @export
huberize <- function(x,lower,upper){
  UseMethod("huberize", x)
}
#' @export
huberize.Distribution <- function(x, lower = NULL, upper = NULL){
  HuberizedDistribution$new(x, lower, upper)
}
