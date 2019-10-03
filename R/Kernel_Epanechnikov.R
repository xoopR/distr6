#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Epanechnikov Kernel
#-------------------------------------------------------------
#' @title Epanechnikov Kernel
#'
#' @description Mathematical and statistical functions for the Epanechnikov kernel defined by the pdf,
#' \deqn{f(x) = \frac{3}{4}(1-x^2)}{f(x) = 3/4(1-x^2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The quantile function is omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Epanechnikov
#'
#' @section Constructor: Epanechnikov$new(decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' }
#'
#' @inheritSection Kernel Public Variables
#' @inheritSection Kernel Public Methods
#'
#' @return Returns an R6 object inheriting from class Kernel.
#'
#' @export
NULL
#-------------------------------------------------------------
# Uniform Kernel Definition
#-------------------------------------------------------------
Epanechnikov <- R6::R6Class("Epanechnikov", inherit = Kernel, lock_objects = F)
Epanechnikov$set("public","name","Epanechnikov")
Epanechnikov$set("public","short_name","Epan")
Epanechnikov$set("public","description","Epanechnikov Kernel")
Epanechnikov$set("public","squared2Norm",function(){
  return(3/5)
})
Epanechnikov$set("public","variance",function(){
  return(1/5)
})
Epanechnikov$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(0.75 * (1-x1^2))
  }
  cdf <- function(x1){
    return(3/4*x1 - 1/4*x1^3 + 1/2)
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf,
                   support = Interval$new(-1, 1),  symmetric = TRUE)
  invisible(self)
})

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Epan", ClassName = "Epanechnikov", Support = "[-1,1]"))
