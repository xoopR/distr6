
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
LogisticKernel <- R6Class("LogisticKernel", inherit = Kernel, lock_objects = F)
LogisticKernel$set("public", "name", "LogisticKernel")
LogisticKernel$set("public", "short_name", "Logis")
LogisticKernel$set("public", "description", "Logistic Kernel")
LogisticKernel$set("public", "squared2Norm", function() {
  return(1 / 6)
})
LogisticKernel$set("public", "variance", function() {
  return(pi^2 / 3)
})
LogisticKernel$set("public", "initialize", function(decorators = NULL) {
  super$initialize(
    decorators = decorators,
    support = Reals$new()
  )
})
LogisticKernel$set("private", ".pdf", function(x, log = FALSE) {
  C_LogisticKernelPdf(x, log)
})
LogisticKernel$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE) {
  C_LogisticKernelCdf(x, lower.tail, log.p)
})
LogisticKernel$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE) {
  C_LogisticKernelQuantile(x, lower.tail, log.p)
})

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Logis", ClassName = "LogisticKernel", Support = "\u211D", Packages = "-"))
