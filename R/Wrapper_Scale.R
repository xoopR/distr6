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

  name = dist$name
  short_name = dist$short_name

  distlist = list(dist)
  names(distlist) = short_name

  self$setScaleMean(mean)
  self$setScaleSd(sd)

  if(!is.null(dist$pdf(1))){
    pdf <- function(x) {}
    body(pdf) <- substitute({
      locationTrafo <- self$wrappedModels(name)$expectation() - self$getScaleMean()
      scaleTrafo <- self$wrappedModels(name)$sd() / self$getScaleSd()
      self$wrappedModels(name)$pdf(x * scaleTrafo + locationTrafo) / scaleTrafo
    }, list(name = short_name))
  } else
    pdf <- NULL
  if(!is.null(dist$cdf(1))){
    cdf <- function(x) {}
    body(cdf) <- substitute({
      locationTrafo <- self$wrappedModels(name)$expectation() - self$getScaleMean()
      scaleTrafo <- self$wrappedModels(name)$sd() / self$getScaleSd()
      self$wrappedModels(name)$cdf(x * scaleTrafo + locationTrafo)
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