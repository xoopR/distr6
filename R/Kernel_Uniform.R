#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Uniform Kernel
#-------------------------------------------------------------
#' @title Uniform Kernel
#'
#' @description Mathematical and statistical functions for the Uniform kernel defined by the pdf,
#' \deqn{f(x) = 1/2}
#' over the support \eqn{x \epsilon (-1,1)}.
#'
#' @name UniformKernel
#'
#' @section Constructor: UniformKernel$new(decorators = NULL)
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
# Uniform Kernel Definition
#-------------------------------------------------------------
UniformKernel <- R6::R6Class("UniformKernel", inherit = Kernel, lock_objects = F)
UniformKernel$set("public","name","UniformKernel")
UniformKernel$set("public","short_name","KUnif")
UniformKernel$set("public","description","Uniform Kernel")
UniformKernel$set("public","variance",function(){
  return(1/3)
})
UniformKernel$set("public","squared2Norm",function(){
  return(0.5)
})
UniformKernel$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(rep(0.5,length(x1)))
  }
  cdf <- function(x1){
    return(0.5*x1 + 0.5)
  }
  quantile <- function(p){
    return(2 * (p - 0.5))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Interval$new(-1, 1),  symmetric = TRUE)
  invisible(self)
})
