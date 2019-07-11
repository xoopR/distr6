#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Normal Kernel
#-------------------------------------------------------------
#' @title Normal Kernel
#'
#' @description Mathematical and statistical functions for the NormalKernel kernel defined by the pdf,
#' \deqn{f(x) = \frac{exp(-\frac{x^2}{2})}{\sqrt(2\pi)}}{f(x) = exp(-x^2/2)/\sqrt(2\pi)}
#' over the support \eqn{x \in \R}{x \epsilon R}.
#'
#' @details We use the \code{erf} and \code{erfinv} error and inverse error functions from the Pracma
#' package.
#'
#' @name NormalKernel
#'
#' @section Constructor: NormalKernel$new(decorators = NULL)
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
# NormalKernel Kernel Definition
#-------------------------------------------------------------
NormalKernel <- R6::R6Class("NormalKernel", inherit = Kernel, lock_objects = F)
NormalKernel$set("public","name","NormalKernel")
NormalKernel$set("public","short_name","NormKern")
NormalKernel$set("public","description","Normal Kernel")
NormalKernel$set("public","squared2Norm",function(){
  return((2*sqrt(pi))^-1)
})
NormalKernel$set("public","variance",function(){
  return(1)
})
NormalKernel$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1/sqrt(2*pi) * exp(-0.5 * x1^2))
  }
  cdf <- function(x1){
    return(1/2 * (pracma::erf(x1/sqrt(2)) + 1))
  }
  quantile <- function(p){
    return(sqrt(2) * pracma::erfinv(2*p - 1))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(),  symmetric = TRUE)
  invisible(self)
})
