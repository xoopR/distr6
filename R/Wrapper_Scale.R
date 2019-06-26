#' @title Distribution Scale Wrapper
#'
#' @description Scales a distribution to a given mean and standard deviation. By default the distribution
#' is centered (\code{mean} = 0) with unit variance (\code{sd} = 1)
#'
#
#' @name Scale
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to scale. \cr
#' \code{mean} \tab numeric \tab Desired mean after distribution shift. \cr
#' \code{sd} \tab numeric \tab Desired standard deviation after distribution scale.
#' }
#'
#' @section Getters:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#' \code{getScaleMean()} \tab numeric \tab Return mean of scaled distribution. \cr
#' \code{getScaleSd()} \tab numeric \tab Return standard deviation of scaled distribution. \cr
#' }
#'
#'@section Setters:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input Type} \tab \strong{Details} \cr
#' \code{setScaleMean(mean)} \tab numeric \tab Set mean to scale distribution to. \cr
#' \code{setScaleSd(sd)} \tab numeric \tab Set standard deviation to scale distribution to. \cr
#' }
#'
#' @examples
#' \dontrun{
#' Scale$new(Binomial$new())
#' }
#'
#' @seealso See \code{\link{DistributionWrapper}} for inherited wrapper methods and see \code{\link{Distribution}}
#' for a full list of inherited distribution methods.

Scale <- R6::R6Class("Scale", inherit = DistributionWrapper, lock_objects = FALSE)
Scale$set("public","initialize",function(dist, mean = 0, sd = 1,...){
  assertDistribution(dist)
  dist = dist$clone()

  name = dist$name
  short_name = dist$short_name

  distlist = list(dist)
  names(distlist) = short_name

  self$setScaleMean(mean)
  self$setScaleSd(sd)

  if(!is.null(dist$pdf(1))){
    pdf <- function(x1) {}
    body(pdf) <- substitute({
      locationTrafo <- self$wrappedModels(name)$expectation() - self$getScaleMean()
      scaleTrafo <- self$wrappedModels(name)$sd() / self$getScaleSd()
      self$wrappedModels(name)$pdf(x1 * scaleTrafo + locationTrafo) / scaleTrafo
    }, list(name = short_name))
  } else
    pdf <- NULL
  if(!is.null(dist$cdf(1))){
    cdf <- function(x1) {}
    body(cdf) <- substitute({
      locationTrafo <- self$wrappedModels(name)$expectation() - self$getScaleMean()
      scaleTrafo <- self$wrappedModels(name)$sd() / self$getScaleSd()
      self$wrappedModels(name)$cdf(x1 * scaleTrafo + locationTrafo)
    }, list(name = short_name))
  } else
    cdf <- NULL

  name = paste("Scaled",name)
  short_name = paste0("Scaled",short_name)

  type = Reals$new()

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, type = type, prefixParams = FALSE,...)
}) # IN PROGRESS

Scale$set("public","getScaleMean",function(){
  return(private$.scaleMean)
})
Scale$set("public","getScaleSd",function(){
  return(private$.scaleSd)
})
Scale$set("public","setScaleMean",function(mean){
  private$.scaleMean <- mean
  invisible(self)
})
Scale$set("public","setScaleSd",function(sd){
  private$.scaleSd <- sd
  invisible(self)
})
Scale$set("private",".scaleMean",0)
Scale$set("private",".scaleSd",1)
