#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Triangular Kernel
#-------------------------------------------------------------
#' @title Triangular Kernel
#'
#' @description Mathematical and statistical functions for the Triangular kernel defined by the pdf,
#' \deqn{f(x) = 1 - |x|}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name TriangularKernel
#'
#' @section Constructor: TriangularKernel$new(decorators = NULL)
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
TriangularKernel <- R6::R6Class("TriangularKernel", inherit = Kernel, lock_objects = F)
TriangularKernel$set("public","name","TriangularKernel")
TriangularKernel$set("public","short_name","Tri")
TriangularKernel$set("public","description","Triangular Kernel")
TriangularKernel$set("public","variance",function(){
  return(1/6)
})
TriangularKernel$set("public","squared2Norm",function(){
  return(2/3)
})
TriangularKernel$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1 - abs(x1))
  }
  cdf <- function(x1){
    cdf = x1
    cdf[x1 < 0] = x1[x1 < 0] + 0.5*x1[x1 < 0]^2 + 1/2
    cdf[x1 == 0] = 0.5
    cdf[x1 > 0] = x1[x1 > 0] - 0.5*x1[x1 > 0]^2 + 1/2
    return(cdf)
  }
  quantile <- function(p){
    quantile = p
    quantile[p < 0.5] = -1 + sqrt(2 * p[p < 0.5])
    quantile[p == 0.5] = 0
    quantile[p > 0.5] = 1 - sqrt(2 - 2*p[p > 0.5])
    return(quantile)
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Interval$new(-1, 1),  symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Tri", ClassName = "TriangularKernel", Support = "[-1,1]"))
