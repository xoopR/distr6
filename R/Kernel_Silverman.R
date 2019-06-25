#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Silverman Kernel
#-------------------------------------------------------------
#' @title Silverman Kernel
#'
#' @description Mathematical and statistical functions for the Silverman kernel defined by the pdf,
#' \deqn{f(x) = 1/2 * exp(-|x1|/sqrt(2)) * sin(|x1|/sqrt(2) + \pi/4)}
#' over the support \eqn{x \epsilon R}.
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
#' @export
NULL
#-------------------------------------------------------------
# Silverman Kernel Definition
#-------------------------------------------------------------
Silverman <- R6::R6Class("Silverman", inherit = Kernel, lock_objects = F)
Silverman$set("public","name","Silverman")
Silverman$set("public","short_name","Silv")
Silverman$set("public","description","Silverman Kernel")
Silverman$set("public","var",function(){

})
Silverman$set("public","squared2Norm",function(){
  return((3 * sqrt(2))/16)
})
Silverman$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return(1/2 * exp(-abs(x1)/sqrt(2)) * sin(abs(x1)/sqrt(2) + pi/4))
  }
  cdf <- function(x1){

  }
  quantile <- function(p){

  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING
