#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Silverman Kernel
#-------------------------------------------------------------
#' @title Silverman Kernel
#'
#' @description Mathematical and statistical functions for the Silverman kernel defined by the pdf,
#' \deqn{f(x) = exp(-|x|/\sqrt{2})/2 * sin(|x|/\sqrt{2} + \pi/4)}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Silverman
#'
#' @section Constructor: Silverman$new(decorators = NULL)
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
# Silverman Kernel Definition
#-------------------------------------------------------------
Silverman <- R6::R6Class("Silverman", inherit = Kernel, lock_objects = F)
Silverman$set("public","name","Silverman")
Silverman$set("public","short_name","Silv")
Silverman$set("public","description","Silverman Kernel")
Silverman$set("public","squared2Norm",function(){
  return((3 * sqrt(2))/16)
})
Silverman$set("public","variance",function(){
  return(0)
})
Silverman$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1/2 * exp(-abs(x1)/sqrt(2)) * sin(abs(x1)/sqrt(2) + pi/4))
  }

  super$initialize(decorators = decorators, pdf = pdf,
                   support = Reals$new(),  symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Silv", ClassName = "Silverman", Support = "\u211D"))
