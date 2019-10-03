#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Sigmoid Kernel
#-------------------------------------------------------------
#' @title Sigmoid Kernel
#'
#' @description Mathematical and statistical functions for the Sigmoid kernel defined by the pdf,
#' \deqn{f(x) = 2/\pi(exp(x) + exp(-x))^{-1}}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Sigmoid
#'
#' @section Constructor: Sigmoid$new(decorators = NULL)
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
# Sigmoid Kernel Definition
#-------------------------------------------------------------
Sigmoid <- R6::R6Class("Sigmoid", inherit = Kernel, lock_objects = F)
Sigmoid$set("public","name","Sigmoid")
Sigmoid$set("public","short_name","Sigm")
Sigmoid$set("public","description","Sigmoid Kernel")
Sigmoid$set("public","squared2Norm",function(){
  return(2 / pi^2)
})
Sigmoid$set("public","variance",function(){
  return(pi^2/4)
})
Sigmoid$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return((2/pi) * (exp(x1) + exp(-x1))^-1)
  }

  super$initialize(decorators = decorators, pdf = pdf, support = Reals$new(),
                   symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Sigm", ClassName = "Sigmoid", Support = "\u211D"))
