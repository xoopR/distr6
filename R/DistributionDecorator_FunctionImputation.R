#' @title Numeric Pdf/Cdf/Quantile/Rand Functions
#'
#' @description Numeric statistical functions including density/mass function, distribution function,
#' inverse distribution and simulation.
#' @name FunctionImputation
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution. The functions are imputed based on
#'  which are already available in the distribution. For example if the pdf is known then this is used
#'  to generate the cdf, quantile and rand. But if the cdf and pdf are both known, then cdf is used to
#'  generate the quantile.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Public Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Input -> Output} \tab \strong{Details} \cr
#' \code{pdf(x1)} \tab numeric x numeric -> numeric \tab Numeric pdf evaluated at x1. \cr
#' \code{cdf(x1)} \tab numeric x numeric -> numeric \tab Numeric cdf evaluated at x1. \cr
#' \code{quantile(p)} \tab numeric x numeric -> numeric \tab Numeric inverse cdf evaluated at x1. \cr
#' \code{rand(n)} \tab integer x numeric -> numeric \tab n simulations from distribution. \cr
#' }
#'
#' @seealso \code{\link{DistributionDecorator}}
#'
#' @examples
#' x = Distribution$new("Test", pdf = function(x) 1/(4-1), support = Interval$new(1,4),
#' type = Reals$new())
#' decorate(x, FunctionImputation)
#' plot(x$pdf(0:5))
#' plot(x$cdf(0:5))
#'
#' @examples
#' x = Distribution$new("Test", pdf = function(x) 1/(4-1), decorators = ExoticStatistics)
#' x$cdf(1)
NULL


#' @export
FunctionImputation <- R6::R6Class("FunctionImputation", inherit = DistributionDecorator)

FunctionImputation$set("public","pdf",function(x1){
  # CDF2PDF
  if(testUnivariate(self)){
      if(testDiscrete(self)){
        return(self$cdf(x1) - self$cdf(x1-1))
      } else if(testContinuous(self)){
        message(.distr6$message_numeric)
        return(as.numeric(attr(deriv(y~self$cdf(x1),"x1", func = TRUE)(x1),"gradient")))
      }
  } else
    return("FunctionImputation is currently only supported for univariate distributions.")
})
FunctionImputation$set("public","cdf",function(x1){
  # PDF2CDF
  if(testUnivariate(self)){
    if(testDiscrete(self)){
      return(sum(self$pdf(self$inf():x1)))
    } else if(testContinuous(self)){
      message(.distr6$message_numeric)
      return(integrate(self$pdf, lower = self$inf(), upper = x1)$value)
    }
  } else
    return("FunctionImputation is currently only supported for univariate distributions.")
})
FunctionImputation$set("public","quantile",function(p){
  message(.distr6$message_numeric)
  if(!RSmisc::testMessage(self$cdf(1))){
    #CDF2QUANTILE - DISCRETE/CONT
    if(testDiscrete(self)){
      to = ifelse(self$sup() == Inf, 1e+08, self$sup())
      from = ifelse(self$inf() == -Inf, -1e+08, self$inf())
      x1 = seq.int(from,to,by = 1)
      y = self$cdf(x1)

      message(.distr6$message_numeric)

      return(sapply(p, function(p0){
        return(x1[min(which(y == min(y[y>p0])))])
      }))

    } else if(testContinuous(self)){
      if(strategy == "inversion"){
        upper = ifelse(self$sup() == Inf, 1e+08, self$sup())
        lower = ifelse(self$inf() == -Inf, -1e+08, self$inf())

        message(.distr6$message_numeric)

        return(sapply(p, function(p0){
          return(GoFKernel::inverse(self$cdf)(p0))
        }))
    }
  }

}})
FunctionImputation$set("public","rand",function(n){
  message(.distr6$message_numeric)
  if(!RSmisc::testMessage(self$quantile(1))){
    # QUANTILE2RAND - DISCRETE/CONT
    return(sapply(1:n, function(x) self$quantile(runif(1))))
  }
  if(!RSmisc::testMessage(self$pdf(1)) & testDiscrete(self)){
    # PDF2RAND - DISCRETE
    return(sample(self$inf():self$sup(), n, TRUE, self$pdf(self$inf():self$sup())))
  }
})
