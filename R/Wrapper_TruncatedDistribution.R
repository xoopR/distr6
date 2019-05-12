#' @name TruncatedDistribution
#' @title Distribution Truncation Wrapper
#' @description A wrapper for truncating any probability distribution at given limits.
#' @seealso \code{\link{HuberizedDistribution}} and \code{\link{DistributionWrapper}} for wrapper details.
#' See \code{\link{Distribution}} for a list of public methods.
#' @details Truncates a distribution at lower and upper limits, using the formulae
#' \deqn{f_T = f_X(x) / (F_X(upper) - F_X(lower))}
#' \deqn{F_T = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
#' where f_T/F_T is the pdf/cdf of the truncated distribution T = Truncate(X, lower, upper) and f_X, F_X is the
#' pdf/cdf of the original distribution.
#'
#' If lower or upper are missing they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#' The support of the new distribution is the interval of points between lower and upper.
#'
#' \code{TruncatedDistribution} inherits all methods from \code{Distribution}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution to truncate. \cr
#' \code{lower} \tab Lower limit for truncation. \cr
#' \code{upper} \tab Upper limit for truncation.
#' }
#'
#' @examples
#' truncBin <- TruncatedDistribution$new(Binomial$new(prob = 0.5, size = 10), lower = 2, upper = 4)
#' truncBin$getParameterValue("truncBin_prob")
NULL

#' @export
TruncatedDistribution <- R6::R6Class("TruncatedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
TruncatedDistribution$set("public","initialize",function(distribution, lower, upper){

  assertDistribution(distribution)

  if(is.null(distribution$cdf(1)))
    stop("cdf is required for truncation. Try decorate(Distribution, FunctionImputation) first.")

  if(!missing(lower) & !missing(upper)){
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / (self$wrappedModels()[[1]]$cdf(upper) - self$wrappedModels()[[1]]$cdf(lower))
    }
    formals(pdf)$self <- self
  } else if(!missing(lower) & missing(upper)){
    upper = distribution$sup()
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / (1 - self$wrappedModels()[[1]]$cdf(lower))
    }
    formals(pdf)$self <- self
  } else if(missing(lower) & !missing(upper)){
    lower = distribution$inf()
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / self$wrappedModels()[[1]]$cdf(lower)
    }
    formals(pdf)$self <- self
  } else{
    lower = distribution$inf()
    upper = distribution$sup()
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / (self$wrappedModels()[[1]]$cdf(upper) - self$wrappedModels()[[1]]$cdf(lower))
    }
    formals(pdf)$self <- self
  }

  name = paste("Truncated",distribution$name)
  short_name = paste0("Truncated",distribution$short_name)

  distlist = list(distribution)
  names(distlist) = distribution$short_name

  super$initialize(distlist = distlist, pdf = pdf, name = name,
                   short_name = short_name, support = Interval$new(lower, upper),
                   type = distribution$type(), prefixParams = FALSE)
}) # IN PROGRESS

#' @title Truncate a Distribution
#' @description S3 functionality to truncate an R6 distribution.
#'
#' @param x distribution to truncate
#' @param lower lower limit for truncation.
#' @param upper upper limit for truncation.
#'
#' @seealso \code{\link{TruncatedDistribution}}
#'
#' @export
truncate <- function(x,lower,upper){
  UseMethod("truncate", x)
}
#' @export
truncate.Distribution <- function(x, lower, upper){
  TruncatedDistribution$new(x, lower, upper)
}