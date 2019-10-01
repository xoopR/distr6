#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Logistic Kernel
#-------------------------------------------------------------
#' @title Logistic Kernel
#'
#' @description Mathematical and statistical functions for the LogisticKernel kernel defined by the pdf,
#' \deqn{f(x) = (exp(x) + 2 + exp(-x))^{-1}}
#' over the support \eqn{x \in R}{x \epsilon R}.
#'
#' @name LogisticKernel
#'
#' @section Constructor: LogisticKernel$new(decorators = NULL)
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
# LogisticKernel Kernel Definition
#-------------------------------------------------------------
LogisticKernel <- R6::R6Class("LogisticKernel", inherit = Kernel, lock_objects = F)
LogisticKernel$set("public","name","LogisticKernel")
LogisticKernel$set("public","short_name","Logis")
LogisticKernel$set("public","description","Logistic Kernel")
LogisticKernel$set("public","squared2Norm",function(){
  return(1/6)
})
LogisticKernel$set("public","variance",function(){
  return(pi^2/3)
})
LogisticKernel$set("public","initialize",function(decorators = NULL){

  pdf <- function(x1){
    return((exp(x1) + 2 + exp(-x1))^-1)
  }
  cdf <- function(x1){
    return(exp(x1)/(exp(x1)+1))
  }
  quantile <- function(p){
    return(-log(-(p-1)/p))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   support = Reals$new(),  symmetric = TRUE)
  invisible(self)
})

.distr6$kernels = rbind(.distr6$kernels, data.table::data.table(ShortName = "Logis", ClassName = "LogisticKernel", Support = "\u211D"))
