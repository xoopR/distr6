MixtureDistribution <- R6::R6Class("MixtureDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
MixtureDistribution$set("public","initialize",function(distlist, weights, ...){

  assertDistributionList(distlist)
  distlist = lapply(distlist, function(x) return(x$clone()))
  distnames = unlist(sapply(distlist, function(x) return(x$short_name())))
  names(distlist) = distnames

  if(missing(weights))
    weights = rep(1/length(distlist), length(distlist))
  else{
    checkmate::assert(length(weights)==length(distlist))
    checkmate::assert(sum(weights)==1)
  }

  pdf <- function(x,...) {
    if(length(x)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$pdf(x)) * weights)))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$pdf(x)) * weights)))
  }
  formals(pdf)$self <- self

  cdf <- function(x,...) {
    if(length(x)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$cdf(x)) * weights)))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$cdf(x)) * weights)))
  }
  formals(cdf)$self <- self

  name = paste("Mixture of",paste(distnames, collapse = "_"))
  short_name = paste0("Mix_",paste(distnames, collapse = "_"))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, ...)
}) # IN PROGRESS
