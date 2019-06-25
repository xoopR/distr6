#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gaussian Kernel
#-------------------------------------------------------------
#' @title Gaussian Kernel
#'
#' @description Mathematical and statistical functions for the Gaussian kernel defined by the pdf,
#' \deqn{f(x) = exp(-x^2/2)/sqrt(2\pi)}
#' over the support \eqn{x \epsilon R}.
#'
#' @name Gaussian
#'
#' @section Constructor: Gaussian$new(decorators = NULL)
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
# Gaussian Kernel Definition
#-------------------------------------------------------------
Gaussian <- R6::R6Class("Gaussian", inherit = Kernel, lock_objects = F)
Gaussian$set("public","name","Gaussian")
Gaussian$set("public","short_name","Gaus")
Gaussian$set("public","description","Gaussian Kernel")
Gaussian$set("public","var",function(){

})
Gaussian$set("public","squared2Norm",function(){
  return((2*sqrt(pi))^-1)
})
Gaussian$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1/sqrt(2*pi) * exp(-0.5 * x1^2))
  }
  cdf <- function(x1){

  }
  quantile <- function(p){

  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING
