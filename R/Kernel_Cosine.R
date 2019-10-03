#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Cosine Kernel
#-------------------------------------------------------------
#' @title Cosine Kernel
#'
#' @description Mathematical and statistical functions for the Cosine kernel defined by the pdf,
#' \deqn{f(x) = (\pi/4)cos(x\pi/2)}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @name Cosine
#'
#' @section Constructor: Cosine$new(decorators = NULL)
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
# Cosine Kernel Definition
#-------------------------------------------------------------
Cosine <- R6::R6Class("Cosine", inherit = Kernel, lock_objects = F)
Cosine$set("public","name","Cosine")
Cosine$set("public","short_name","Cos")
Cosine$set("public","description","Cosine Kernel")
Cosine$set("public","squared2Norm",function(){
  return(pi^2 / 16)
})
Cosine$set("public","variance",function(){
  return(1 - 8/(pi^2))
})
Cosine$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(pi/4 * cos(pi/2 * x1))
  }
  cdf <- function(x1){
    return(0.5 * (sin((pi*x1)/2)+1))
  }
  quantile <- function(p){
    return((2*asin(2*p - 1))/pi)
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Interval$new(-1,1),  symmetric = TRUE)
  invisible(self)
})

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Cos", ClassName = "Cosine", Support = "[-1,1]"))
