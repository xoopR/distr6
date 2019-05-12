#' @name HuberizedDistribution
#' @title Distribution Huberized Wrapper
#' @description A wrapper for huberizing any probability distribution at given limits.
#' @seealso \code{\link{TruncatedDistribution}} and \code{\link{DistributionWrapper}} for wrapper details.
#' See \code{\link{Distribution}} for a list of public methods.
#' @details Huberizes a distribution at lower and upper limits, using the formula
#' \tabular{lll}{
#'  \tab \eqn{F(x)} \tab if \eqn{x \le lower} \cr
#'  f_H(x) = \tab \eqn{f(x)} \tab if \eqn{lower < x < upper} \cr
#'  \tab \eqn{1 - F(x)} \tab if \eqn{x \ge upper} \cr
#' }
#' where f_H is the pdf of the truncated distribution H = Huberize(X, lower, upper) and f_X, F_X is the
#' pdf/cdf of the original distribution.
#'
#' If lower or upper are missing they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#'
#' \code{HuberizedDistribution} inherits all methods from \code{Distribution}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution to huberize. \cr
#' \code{lower} \tab Lower limit for huberization. \cr
#' \code{upper} \tab Upper limit for huberization.
#' }
#'
#' @section Public Methods:
#' \tabular{ll}{
#' \code{getLowerLimit()} \tab Gets lower limit of huberization. \cr
#' \code{getUpperLimit()} \tab Gets upper limit of huberization. \cr
#' }
#'
#' @examples
#' hubBin <- HuberizedDistribution$new(Binomial$new(prob = 0.5, size = 10), lower = 2, upper = 4)
#' hubBin$getParameterValue("Binom_prob")
#' hubBin$getLowerLimit()
#' hubBin$pdf(2)
NULL

#' @export
HuberizedDistribution <- R6::R6Class("HuberizedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
HuberizedDistribution$set("private", ".cutoffInterval", NULL)
HuberizedDistribution$set("public", "getLowerLimit", function(){
  return(private$.cutoffInterval[[1]])
})
HuberizedDistribution$set("public", "getUpperLimit", function(){
  return(private$.cutoffInterval[[2]])
})
HuberizedDistribution$set("public","initialize",function(distribution, lower, upper){

  assertDistribution(distribution)

  if(missing(lower)) lower = distribution$inf()
  if(missing(upper)) upper = distribution$sup()

  pdf <- function(x, ...){
    if(x <= self$getLowerLimit())
      return(self$wrappedModels()[[1]]$cdf(self$getLowerLimit()))
    else if(x >= self$getUpperLimit())
      return(1-self$wrappedModels()[[1]]$cdf(self$getUpperLimit()))
    else
      return(self$wrappedModels()[[1]]$pdf(x))
  }

  name = paste("Huberized",distribution$name)
  short_name = paste0("Huberized",distribution$short_name)

  distlist = list(distribution)
  names(distlist) = distribution$short_name

  private$.cutoffInterval = c(lower, upper)

  super$initialize(distlist = distlist, pdf = pdf, name = name,
                   short_name = short_name, type = distribution$type(),
                   support = distribution$support(), distrDomain = distribution$distrDomain(),
                   prefixParams = FALSE)
}) # IN PROGRESS

#' @export
huberize <- function(x,lower,upper,...){
  UseMethod("huberize", x)
}
#' @export
huberize.Distribution <- function(x, lower, upper,...){
  HuberizedDistribution$new(x, lower, upper)
}