#' @name FunctionImputation
#'
#' @title Imputed Pdf/Cdf/Quantile/Rand Functions
#'
#' @description This decorator imputes missing pdf/cdf/quantile/rand methods from R6 Distributions
#' by using strategies dependent on which methods are already present in the distribution.
#'
#' @details Decorator objects add functionality to the given Distribution object by copying methods
#' in the decorator environment to the chosen Distribution environment. See the 'Added Methods' section
#' below to find details of the methods that are added to the Distribution. Methods already
#' present in the distribution are not overwritten by the decorator.
#'
#' Use \code{\link{decorate}} to decorate a Distribution.
#'
#' All methods in this decorator use numerical approximations and therefore better results may be available
#' from analytic computations.
#'
#' @section Constructor: FunctionImputation$new(distribution)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab Density/mass function \tab \code{\link{pdf}} \cr
#' \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab Distribution function \tab \code{\link{cdf}}\cr
#' \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab Quantile function \tab \code{\link{quantile.Distribution}} \cr
#' \code{rand(n, simplify = TRUE)} \tab Simulation function \tab \code{\link{rand}} \cr
#' }
#'
#' @seealso \code{\link{decorate}}, \code{\link{listDecorators}}
#'
#' @return Returns a decorated R6 object inheriting from class SDistribution with d/p/q/r numerically
#' imputed if previously missing.
#'
#' @examples
#' x = Distribution$new("Test", pdf = function(x) 1/(4-1),
#' support = Interval$new(1,4),
#' type = Reals$new())
#' decorate(x, FunctionImputation)
#' x$pdf(0:5)
#' x$cdf(0:5)
#'
#' @export
NULL

FunctionImputation <- R6::R6Class("FunctionImputation", inherit = DistributionDecorator)
.distr6$decorators <- append(.distr6$decorators, list(FunctionImputation = FunctionImputation))


FunctionImputation$set("public","pdf",function(x1){
  # CDF2PDF
  if(testUnivariate(self)){
      if(testDiscrete(self)){
        return(self$cdf(x1) - self$cdf(x1-1))
      } else if(testContinuous(self)){
        message(.distr6$message_numeric)
        return(pracma::fderiv(self$cdf,x1))
      }
  }
})
FunctionImputation$set("public","cdf",function(x1){
  # PDF2CDF
  if(testUnivariate(self)){
    if(testDiscrete(self)){
      if(length(x1)>1)
        return(sapply(x1,function(x) sum(self$pdf(self$inf():x))))
      else
        return(sum(self$pdf(self$inf():x1)))
    } else if(testContinuous(self)){
      message(.distr6$message_numeric)
      if(length(x1)>1)
        return(unlist(sapply(x1, function(x0) integrate(self$pdf, lower = self$inf(), upper = x0)$value)))
      else
        return(integrate(self$pdf, lower = self$inf(), upper = x1)$value)

    }
  }
})
FunctionImputation$set("public","quantile",function(p){
  message(.distr6$message_numeric)

 # if(!testMessage(self$cdf(1))){
    #CDF2QUANTILE - DISCRETE/CONT
    if(testDiscrete(self)){
      to = ifelse(self$sup() == Inf, 1e+08, self$sup())
      from = ifelse(self$inf() == -Inf, -1e+08, self$inf())
      x1 = seq.int(from,to,by = 1)
      y = self$cdf(x1)

      if(length(p)>1)
        return(unlist(sapply(p, function(p0) return(x1[min(which(y == min(y[y>p0])))]))))
      else
        return(x1[min(which(y == min(y[y>p])))])

    } else if(testContinuous(self)){
 #     if(strategy == "inversion"){
        upper = ifelse(self$sup() == Inf, 1e+08, self$sup())
        lower = ifelse(self$inf() == -Inf, -1e+08, self$inf())

        if(length(p)>1)
          return(unlist(sapply(p, function(p0)
            return(suppressMessages(GoFKernel::inverse(self$cdf,lower = self$inf(),upper = self$sup())(p0))))))
        else
          return(suppressMessages(GoFKernel::inverse(self$cdf, lower = self$inf(), upper = self$sup())(p)))
    }
 #   }
 # }
})
FunctionImputation$set("public","rand",function(n){
  strategy = "q2r"
  if(strategy == "q2r"){
    message(.distr6$message_numeric)
    return(suppressMessages(sapply(1:n, function(x) self$quantile(runif(1)))))
  } #else if(strategy == "p2r"){
  #   message(.distr6$message_numeric)
  #   if(testDiscrete(self))
  #     return(sample(self$inf():self$sup(), n, TRUE, self$pdf(self$inf():self$sup())))
  # }
})
