
#' @title Distribution Scale Wrapper
#'
#' @description Scales a distribution to a given mean and standard deviation. By default the distribution
#' is centered (\code{mean} = 0) with unit variance (\code{sd} = 1)
#'
#' @details STILL IN DEVELOPMENT. Results are likely sub-optimal.
#'
#' @name Scale
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to scale. \cr
#' \code{mean} \tab numeric \tab Desired mean after distribution scale. \cr
#' \code{sd} \tab numeric \tab Desired standard deviation after distribution scale.\cr
#' \code{var}\tab numeric \tab Desired variance after distribution scale.\cr
#' }
#'
#' @section getParameterValue:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#' \code{getParameterValue("scaledmean")} \tab numeric \tab Return mean of scaled distribution. \cr
#' \code{getParameterValue("scaledsd")} \tab numeric \tab Return standard deviation of scaled distribution.\cr
#' \code{getParameterValue("scaledvar")} \tab numeric \tab Return variance of scaled distribution.\cr
#' }
#'
#'@section setParameterValue:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input Type} \tab \strong{Details} \cr
#' \code{setParameterValue(scaledmean=value)} \tab numeric \tab Set the mean of scale distribution to value. \cr
#' \code{setParameterValue(scaledsd=value))} \tab numeric \tab Set the standard deviation of scale distribution to a value. \cr
#' \code{setParameterValue(scaledvar=value))} \tab numeric \tab Set the variance of scale distribution to a value. \cr
#' }
#'
#' @examples
#' \dontrun{
#' Scale$new(Binomial$new())
#' }
#'
#' @seealso See \code{\link{DistributionWrapper}} for inherited wrapper methods and see \code{\link{Distribution}}
#' for a full list of inherited distribution methods.
#' @export
NULL
Scale <- R6::R6Class("Scale", inherit = DistributionWrapper, lock_objects = FALSE)
Scale$set("public","initialize",function(dist, mean = 0, sd = 1, var = NULL,...){
  assertDistribution(dist)
  dist = dist$clone(deep = TRUE)
  
  name = dist$name
  short_name = dist$short_name
  
  distlist = list(dist)
  names(distlist) = short_name
  
  if(!is.null(var))
    sd = sqrt(var)
  else
    var = sd^2
  
  private$.outerParameters <- ParameterSet$new(id = list("scaledmean","scaledsd","scaledvar"),
                                               value = list(mean, sd, var),
                                               support = list(Reals$new(), PosReals$new(), PosReals$new()),
                                               settable = list(FALSE, FALSE, FALSE),
                                               updateFunc = list(NA, NA,
                                                                 function(self) self$getParameterValue("scaledsd")^2),
                                               description = list("Mean of the output scaled distribution.",
                                                                  "Standard deviation of the output scaled distribution.",
                                                                  "Variance of the output scaled distribution"))
  
  if(dist$.__enclos_env__$private$.isPdf){
    pdf <- function(x1){}
    body(pdf) <- substitute({
      scaleTrafo <- self$getParameterValue("scaledsd")/self$wrappedModels()[[1]]$stdev()
      self$wrappedModels()[[1]]$pdf(-self$getParameterValue("scaledmean")+(x1+self$wrappedModels()[[1]]$mean())*scaleTrafo)
    }, list(name = short_name))
  } else
    pdf <- NULL
  
  if(dist$.__enclos_env__$private$.isCdf){
    cdf <- function(x1){}
    body(cdf) <- substitute({
      scaleTrafo <- self$getParameterValue("scaledsd")/self$wrappedModels(name)$stdev()
      self$wrappedModels(name)$cdf(-self$getParameterValue("scaledmean")+(x1+self$wrappedModels(name)$mean())*scaleTrafo)
    }, list(name = short_name))
  } else
    cdf <- NULL
  
  name = paste("Scaled",name)
  short_name = paste0("Scaled",short_name)
  
  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, type = Reals$new(),
                   support = Reals$new(),...)
}) # IN PROGRESS


Scale$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("scaledsd"%in%names(lst) & "scaledvar"%in%names(lst)){
    checkmate::assert(lst[["scaledsd"]]^2 == lst[["scaledvar"]])
  }else if("scaledsd"%in%names(lst)){
    lst[["scaledvar"]] = lst[["scaledsd"]]^2
  }else if("scaledvar"%in%names(lst)){
    lst[["scaledsd"]] = sqrt(lst[["scaledvar"]])
  }
  super$setParameterValue(lst=lst,error=error)
})

