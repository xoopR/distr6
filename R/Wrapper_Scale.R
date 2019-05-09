#' @title Distribution Scale Wrapper
#'
#' @description Scales a distribution to a given mean and standard deviation. By default the distribution
#' is centered (\code{mean} = 0) with unit variance (\code{sd} = 1)
#'
#
#' @name Scale
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution object. \cr
#' \code{mean} \tab desired mean after distribution shift. \cr
#' \code{sd} \tab desired standard deviation after distribution scale.
#' }
#
#'
#' @examples
#' Scale$new(Binomial$new())
#'
#' @seealso See \code{\link{DistributionWrapper}} for inherited wrapper methods and see \code{\link{Distribution}}
#' for a full list of inherited distribution methods.
NULL

#' @export
Scale <- R6::R6Class("Scale", inherit = DistributionWrapper, lock_objects = FALSE)
Scale$set("public","initialize",function(dist, mean = 0, sd = 1,...){
  assertDistribution(dist)
  dist = dist$clone()

  name = dist$name()
  short_name = dist$short_name()

  distlist = list(dist)
  names(distlist) = short_name

  if(!is.null(dist$pdf(1))){
    pdf <- function(x) {}
    body(pdf) <- substitute({
      dist$pdf((x - self$getInternalModel(name)$sd()) + self$getInternalModel(name)$expectation())
    }, list(name = short_name))
  } else
    pdf <- NULL
  if(!is.null(dist$cdf(1))){
    cdf <- function(x) {}
    body(cdf) <- substitute({
      dist$cdf((x*self$getInternalModel(name)$sd()) + self$getInternalModel(name)$expectation())
    }, list(name = short_name))
  } else
    cdf <- NULL

  name = paste("Scaled",name)
  short_name = paste0("Scaled",short_name)

  type = reals$new()

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, type = type, ...)
}) # IN PROGRESS