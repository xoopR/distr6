#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Tricube Kernel
#-------------------------------------------------------------
#' @title Tricube Kernel
#'
#' @description Mathematical and statistical functions for the Tricube kernel defined by the pdf,
#' \deqn{f(x) = 70/81(1 - |x|^3)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Tricube
#'
#' @section Constructor: Tricube$new(decorators = NULL)
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
# Tricube Kernel Definition
#-------------------------------------------------------------
Tricube <- R6::R6Class("Tricube", inherit = Kernel, lock_objects = F)
Tricube$set("public","name","Tricube")
Tricube$set("public","short_name","Tric")
Tricube$set("public","description","Tricube Kernel")
Tricube$set("public","squared2Norm",function(){
  return(175/247)
})
Tricube$set("public","variance",function(){
  return(35/243)
})
Tricube$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(70/81 * (1-abs(x1)^3)^3)
  }

  super$initialize(decorators = decorators, pdf = pdf,
                   support = Interval$new(-1, 1),  symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Tric", ClassName = "Tricube", Support = "[-1,1]"))
