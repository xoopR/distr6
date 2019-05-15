#' @title Numerical Pdf/Cdf/Quantile/Rand Functions
#'
#' @description Numeric statistical functions including density/mass function, distribution function,
#' inverse distribution and simulation.
#' @name FunctionImputation
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution. See the help pages for the individual
#'  CoreStatistics methods to learn more.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @seealso \code{\link{DistributionDecorator}}
#'
#' @examples
#' x = Exponential$new()
#' decorate(x, ExoticStatistics, R62S3 = FALSE)
#' x$survival(1)
#'
#' @examples
#' x = Exponential$new(decorators = ExoticStatistics, R62S3 = FALSE)
#' x$survival(4)
NULL


#' @export
FunctionImputation <- R6::R6Class("FunctionImputation", inherit = DistributionDecorator)

FunctionImputation$set("public","pdf",function(x, log = FALSE){

  if(!testMessage(self$cdf(1))){
  # CDF2PDF
    if(testDiscrete(self)){
      pdf <- self$cdf(x) - self$cdf(x-1)
    } else if(testContinuous(self)){
      message(.distr6$message_numeric)
      pdf <- as.numeric(attr(deriv(y~self$cdf(x),"x", func = TRUE)(x),"gradient"))
    }
  }
  # RAND2PDF
})
FunctionImputation$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE){

})
FunctionImputation$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  if(!testMessage(self$cdf(1))){
    #CDF2QUANTILE - DISCRETE/CONT
    if(testDiscrete(self)){
      to = ifelse(self$sup() == Inf, 1e+08, self$sup())
      from = ifelse(self$inf() == -Inf, -1e+08, self$inf())
      x = seq.int(from,to,by = 1)
      y = self$cdf(x)

      message(.distr6$message_numeric)

      return(sapply(p, function(p0){
        return(x[min(which(y == min(y[y>p0])))])
      }))

    } else if(testContinuous(self)){
      if(strategy == "inversion"){
        upper = ifelse(self$sup() == Inf, 1e+08, self$sup())
        lower = ifelse(self$inf() == -Inf, -1e+08, self$inf())

        message(.distr6$message_numeric)

        return(sapply(p, function(p0){
          return(GoFKernel::inverse(self$cdf)(p0))
        }))
      } else if(stratgy == "grid"){

      }

    }
  }

})
FunctionImputation$set("public","rand",function(n){
  if(!testMessage(self$quantile(1))){
    # QUANTILE2RAND - DISCRETE/CONT
    return(sapply(1:n, function(x) self$quantile(runif(1))))
  }
  if(!testMessage(self$pdf(1))){
    # PDF2RAND - DISCRETE
    sample(self$inf():self$sup(), n, TRUE, self$pdf(self$inf():self$sup()))
  }
})