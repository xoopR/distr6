#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Sigmoid Kernel
#-------------------------------------------------------------
#' @title Sigmoid Kernel
#'
#' @description Mathematical and statistical functions for the Sigmoid kernel defined by the pdf,
#' \deqn{f(x) = (2/\pi) * (exp(x) + exp(-x))^-1}
#' over the support \eqn{x \epsilon R}.
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
#' @export
NULL
#-------------------------------------------------------------
# Sigmoid Kernel Definition
#-------------------------------------------------------------
Sigmoid <- R6::R6Class("Sigmoid", inherit = Kernel, lock_objects = F)
Sigmoid$set("public","name","Sigmoid")
Sigmoid$set("public","short_name","Sigm")
Sigmoid$set("public","description","Sigmoid Kernel")
Sigmoid$set("public","var",function(){

})
Sigmoid$set("public","squared2Norm",function(){
  return(2 / pi^2)
})
Sigmoid$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return((2/pi) * (exp(x1) + exp(-x1))^-1)
  }
  cdf <- function(x1){

  }
  quantile <- function(p){

  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING
