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
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
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
HuberizedDistribution$set("public","initialize",function(distribution, lower = NULL, upper = NULL){

  assertDistribution(distribution)

  if(!distribution$.__enclos_env__$private$.isPdf | !distribution$.__enclos_env__$private$.isCdf)
    stop("pdf and cdf are required for huberization. Try decorate(Distribution, FunctionImputation) first.")

  if(is.null(lower)) lower = distribution$inf()
  if(is.null(upper)) upper = distribution$sup()

  name = paste("Huberized",distribution$name)
  short_name = paste0("Huberized",distribution$short_name)

  distlist = list(distribution)
  names(distlist) = distribution$short_name

  description = paste0(distribution$description, " Huberized between ",lower," and ",upper,".")

  cdf <- function(x1){
    cdf = x1
    if(any(x1 == self$inf()))
      cdf[x1 == self$inf()] <- rep(self$wrappedModels()[[1]]$cdf(self$inf()), sum(x1 == self$inf()))
    if(any(x1 == self$sup()))
      cdf[x1 == self$sup()] <- rep(1, sum(x1 == self$sup()))
    if(any(x1 > self$inf() & x1 < self$sup()))
      cdf[x1 > self$inf() & x1 < self$sup()] <- self$wrappedModels()[[1]]$cdf(cdf[x1 > self$inf() & x1 < self$sup()])

    return(cdf)
  }

  quantile <- function(p){
    p = self$wrappedModels()[[1]]$quantile(p)
    quantile = p
    if(any(p <= self$inf()))
      quantile[p <= self$inf()] = self$inf()
    if(any(p >= self$inf()))
      quantile[p >= self$sup()] = self$sup()
    if(any(p < self$sup() & p > self$inf()))
      quantile[p < self$sup() & p > self$inf()] = p[p < self$sup() & p > self$inf()]

    return(quantile)
  }

  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  if(testDiscrete(distribution)){

    support <- Set$new(lower:upper)

    pdf <- function(x1){
      pdf = x1
      if(any(x1 == self$inf()))
        pdf[x1 == self$inf()] <- rep(self$wrappedModels()[[1]]$cdf(self$inf()), sum(x1 == self$inf()))
      if(any(x1 == self$sup()))
        pdf[x1 == self$sup()] <- rep(self$wrappedModels()[[1]]$cdf(self$sup(), lower.tail = F) +
          self$wrappedModels()[[1]]$pdf(self$sup()), sum(x1 == self$sup()))
      if(any(x1 > self$inf() & x1 < self$sup()))
        pdf[x1 > self$inf() & x1 < self$sup()] <- self$wrappedModels()[[1]]$pdf(pdf[x1 > self$inf() & x1 < self$sup()])

      return(pdf)
    }

    super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                     name = name, short_name = short_name, type = distribution$type(),
                     support = support, distrDomain = distribution$distrDomain(),
                     prefixParams = FALSE, description = description,
                     valueSupport = "mixture")
  }else if(testContinuous(distribution)){
    support <- Interval$new(lower, upper)

    super$initialize(distlist = distlist, cdf = cdf, quantile = quantile, rand = rand, name = name,
                     short_name = short_name, type = distribution$type(),
                     support = support, distrDomain = distribution$distrDomain(),
                     prefixParams = FALSE, description = description,
                     valueSupport = "mixture")
  } else
    stop(.distr6$huberize_discrete)
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
  if(testDiscrete(x) | testContinuous(x))
    HuberizedDistribution$new(x, lower, upper)
  else
    message(.distr6$huberize_discrete)
}
