#' @title Distribution Scale Wrapper
#'
#' @description Scales a distribution to a given mean and standard deviation. By default the distribution
#' is centered (\code{mean} = 0) with unit variance (\code{sd} = 1)
#'
#' @details STILL IN DEVELOPMENT.
#
#' @name Scale
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to scale. \cr
#' \code{mean} \tab numeric \tab Desired mean after distribution shift. \cr
#' \code{sd} \tab numeric \tab Desired standard deviation after distribution scale.
#' }
#'
#' @seealso See \code{\link{DistributionWrapper}} for inherited wrapper methods and see \code{\link{Distribution}}
#' for a full list of inherited distribution methods.

Scale <- R6::R6Class("Scale", inherit = DistributionWrapper, lock_objects = FALSE)
Scale$set("public","initialize",function(dist, mean = 0, sd = 1){
  assertDistribution(dist)
  dist = dist$clone()

  name = dist$name
  short_name = dist$short_name

  distlist = list(dist)
  names(distlist) = short_name

  # if(!is.null(dist$pdf(1))){
  #   pdf <- function(x1) {}
  #   body(pdf) <- substitute({
  #     locationTrafo <- self$wrappedModels(name)$mean() - self$getParameterValue("scaledMean")
  #     scaleTrafo <- self$wrappedModels(name)$stdev() / self$getParameterValue("scaledSD")
  #     self$wrappedModels(name)$pdf(x1 * scaleTrafo + locationTrafo) / scaleTrafo
  #   }, list(name = short_name))
  # } else
    pdf <- NULL
  if(!is.null(dist$cdf(1))){
    cdf <- function(x1) {}
    body(cdf) <- substitute({
      locationTrafo <- self$wrappedModels(name)$mean() - self$getParameterValue("scaledMean")
      scaleTrafo <- self$wrappedModels(name)$stdev() / self$getParameterValue("scaledSD")
      self$wrappedModels(name)$cdf((x1 - locationTrafo)/scaleTrafo)
    }, list(name = short_name))
  } else
    cdf <- NULL

  name = paste("Scaled",name)
  short_name = paste0("Scaled",short_name)

  private$.outerParameters <- ParameterSet$new(id = list("scaledMean", "scaledSD"),
                                               value = list(mean, sd),
                                               support = list(Reals$new(), PosReals$new()),
                                               settable = list(FALSE, FALSE),
                                               description = list("Scaled Mean.",
                                                                  "Scaled standard deviation."))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, type = dist$type())
})
