#' @name HuberizedDistribution
#' @title Distribution Huberization Wrapper
#' @description A wrapper for huberizing any probability distribution at given limits.
#'
#' @details Huberizes a distribution at lower and upper limits, using the formula
#'
#' \eqn{f_H(x) = F(x), if x \le lower}
#'
#' \eqn{f_H(x) = f(x), if lower < x < upper}
#'
#' \eqn{f_H(x) = F(x), if x \ge upper}
#'
#' where f_H is the pdf of the truncated distribution H = Huberize(X, lower, upper) and \eqn{f_X}/\eqn{F_X} is the
#' pdf/cdf of the original distribution.
#'
#' If lower or upper are NULL they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#'
#' The pdf and cdf of the distribution are required for this wrapper, if unavailable decorate with
#' \code{FunctionImputation} first.
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
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}}, \code{\link{FunctionImputation}}, \code{\link{huberize}}
#'
#' @return Returns an R6 object of class HuberizedDistribution.
#'
#' @examples
#' hubBin <- HuberizedDistribution$new(
#'           Binomial$new(prob = 0.5, size = 10),
#'           lower = 2, upper = 4)
#' hubBin$getParameterValue("prob")
#' hubBin$pdf(2)
#'
#' @export
NULL
HuberizedDistribution <- R6::R6Class("HuberizedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
.distr6$wrappers <- append(.distr6$wrappers, list(HuberizedDistribution = HuberizedDistribution))

HuberizedDistribution$set("public","initialize",function(distribution, lower = NULL, upper = NULL){

  assertDistribution(distribution)

  if(!distribution$.__enclos_env__$private$.isPdf | !distribution$.__enclos_env__$private$.isCdf)
    stop("pdf and cdf are required for huberization. Try decorate(Distribution, FunctionImputation) first.")

  if(is.null(lower)) lower = distribution$inf()
  if(is.null(upper)) upper = distribution$sup()

  name = paste("Huberized",distribution$name)
  short_name = paste0("Hub",distribution$short_name)

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
    if(any(p >= self$sup()))
      quantile[p >= self$sup()] = self$sup()
    if(any(p < self$sup() & p > self$inf()))
      quantile[p < self$sup() & p > self$inf()] = p[p < self$sup() & p > self$inf()]

    return(quantile)
  }

  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  private$.outerParameters <- ParameterSet$new(id = list("hubLower", "hubUpper"), value = list(lower, upper),
                         support = list(Reals$new(), Reals$new()), settable = list(FALSE, FALSE),
                         description = list("Lower limit of huberization.",
                                            "Upper limit of huberization."))

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
                     support = support,
                     description = description,
                     valueSupport = "mixture")
  }else if(testContinuous(distribution)){
    support <- Interval$new(lower, upper)

    super$initialize(distlist = distlist, cdf = cdf, quantile = quantile, rand = rand, name = name,
                     short_name = short_name, type = distribution$type(),
                     support = support,
                     description = description,
                     valueSupport = "mixture")
  } else
    stop(.distr6$huberize_discrete)
})
HuberizedDistribution$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)

  if("hubLower" %in% names(lst) & "hubUpper" %in% names(lst))
    checkmate::assert(lst[["hubLower"]] < lst[["hubUpper"]], .var.name = "hubLower must be < hubUpper")
  else if("hubLower" %in% names(lst))
    checkmate::assert(lst[["hubLower"]] < self$getParameterValue("hubUpper"), .var.name = "hubLower must be < hubUpper")
  else if("hubUpper" %in% names(lst))
    checkmate::assert(lst[["hubUpper"]] > self$getParameterValue("hubLower"), .var.name = "hubUpper must be > hubLower")


  super$setParameterValue(lst = lst, error = error)
  if(inherits(self$support(),"Set"))
    private$.properties$support <- Set$new(self$getParameterValue("hubLower"):self$getParameterValue("hubUpper"))
  else
    private$.properties$support <- Interval$new(self$getParameterValue("hubLower"), self$getParameterValue("hubUpper"))

  invisible(self)
})

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
