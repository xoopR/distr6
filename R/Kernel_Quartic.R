#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Quartic Kernel
#-------------------------------------------------------------
#' @title Quartic Kernel
#'
#' @description Mathematical and statistical functions for the Quartic kernel defined by the pdf,
#' \deqn{f(x) = 15/16(1 - x^2)^2}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details Quantile is omitted as no closed form analytic expression could be found, decorate with
#' FunctionImputation for numeric results.
#'
#' @name Quartic
#'
#' @section Constructor: Quartic$new(decorators = NULL)
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
Quartic <- R6::R6Class("Quartic", inherit = Kernel, lock_objects = F)
Quartic$set("public","name","Quartic")
Quartic$set("public","short_name","Quart")
Quartic$set("public","description","Quartic Kernel")
Quartic$set("public","squared2Norm",function(){
  return(5/7)
})
Quartic$set("public","variance",function(){
  return(1/7)
})
Quartic$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(15/16 * (1-x1^2)^2)
  }
  cdf <- function(x1){
    return(15/16 * (x1 - 2/3*x1^3 + 1/5*x1^5 + 8/15))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf,
                   support = Interval$new(-1, 1),  symmetric = TRUE)
  invisible(self)
}) # QUANTILE & VAR MISSING

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Quart", ClassName = "Quartic", Support = "[-1,1]"))
