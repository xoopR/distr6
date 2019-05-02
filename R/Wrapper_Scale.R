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