TruncatedDistribution <- R6::R6Class("TruncatedDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
TruncatedDistribution$set("public","initialize",function(distribution, lower, upper){

  assertDistribution(distribution)

  if(!missing(lower) & !missing(upper)){
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / (self$wrappedModels()[[1]]$cdf(upper) - self$wrappedModels()[[1]]$cdf(lower))
    }
    formals(pdf)$self <- self
  } else if(!missing(lower) & missing(upper)){
    pdf <- function(x,...) {
      self$wrappedModels()[[1]]$pdf(x) / (1 - self$wrappedModels()[[1]]$cdf(lower))
    }
    formals(pdf)$self <- self
  } else if(missing(lower) & !missing(upper)){
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

  name = paste("Truncated",distribution$name())
  short_name = paste0("Truncated",distribution$short_name())

  distlist = list(distribution)
  names(distlist) = distribution$short_name()

  super$initialize(distlist = distlist, pdf = pdf, name = name,
                   short_name = short_name, support = interval$new(lower, upper),
                   type = distribution$type())
}) # IN PROGRESS
