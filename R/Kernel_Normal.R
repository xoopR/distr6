#-------------------------------------------------------------
# Normal Kernel
#-------------------------------------------------------------
#' @title Normal Kernel
#'
#' @description Mathematical and statistical functions for the NormalKernel kernel defined by the pdf,
#' \deqn{f(x) = exp(-x^2/2)/\sqrt{2\pi}}
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
#' @return Returns an R6 object inheriting from class Kernel.
#'
#' @export
NULL
#-------------------------------------------------------------
# NormalKernel Kernel Definition
#-------------------------------------------------------------
NormalKernel <- R6Class("NormalKernel", inherit = Kernel, lock_objects = F,
                        public = list(
                          name = "NormalKernel",
                          short_name = "Norm",
                          description = "Normal Kernel",
                          package = "pracma",

                          initialize = function(decorators = NULL){
                            super$initialize(decorators = decorators,
                                             support = Reals$new())
                          },

                          squared2Norm = function(){
                            return((2*sqrt(pi))^-1)
                          },

                          variance = function(){
                            return(1)
                          }
                        ),

                        private = list(
                          .pdf = function(x){
                            return(1/sqrt(2*pi) * exp(-0.5 * x^2))
                          },
                          .cdf = function(x){
                            return(1/2 * (pracma::erf(x/sqrt(2)) + 1))
                          },
                          .quantile = function(p){
                            return(sqrt(2) * pracma::erfinv(2*p - 1))
                          }
                        )
                        )
