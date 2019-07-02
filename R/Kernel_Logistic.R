#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# KLogistic Kernel
#-------------------------------------------------------------
#' @title Logistic Kernel
#'
#' @description Mathematical and statistical functions for the KLogistic kernel defined by the pdf,
#' \deqn{f(x) = (exp(x) + 2 + exp(-x))^-1}
#' over the support \eqn{x \epsilon R}.
#'
#' @name KLogistic
#'
#' @section Constructor: KLogistic$new(decorators = NULL)
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
# KLogistic Kernel Definition
#-------------------------------------------------------------
KLogistic <- R6::R6Class("KLogistic", inherit = Kernel, lock_objects = F)
KLogistic$set("public","name","KLogistic")
KLogistic$set("public","short_name","KLogis")
KLogistic$set("public","description","KLogistic Kernel")
KLogistic$set("public","squared2Norm",function(){
  return(1/6)
})
KLogistic$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return((exp(x1) + 2 + exp(-x1))^-1)
  }
  cdf <- function(x1){

  }
  quantile <- function(p){

  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(), distrDomain = Reals$new(), symmetric = TRUE)
  invisible(self)
}) # CDF, QUANTILE & VAR MISSING
