#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Triangular Kernel
#-------------------------------------------------------------
#' @title Triangular Kernel
#'
#' @description Mathematical and statistical functions for the Triangular kernel defined by the pdf,
#' \deqn{f(x) = 1 - |x|}
#' over the support \eqn{x \epsilon (-1,1)}.
#'
#' @name KTriangular
#'
#' @section Constructor: KTriangular$new(decorators = NULL)
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
KTriangular <- R6::R6Class("KTriangular", inherit = Kernel, lock_objects = F)
KTriangular$set("public","name","KTriangular")
KTriangular$set("public","short_name","KTri")
KTriangular$set("public","description","Triangular Kernel")
KTriangular$set("public","squared2Norm",function(){
  return(2/3)
})
KTriangular$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1 - abs(x1))
  }
  cdf <- function(x1){

  }
  quantile <- function(p){

  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Interval$new(-1, 1), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING
