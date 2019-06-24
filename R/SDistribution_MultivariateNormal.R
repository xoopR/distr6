#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title MultivariateNormal Distribution
#'
#' @description Mathematical and statistical functions for the MultivariateNormal distribution parameterised
#' with size and probabilites and defined by the pmf,
#' \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' where \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1} are the probabilities for each of the \eqn{K} categories and
#' \eqn{n = 1,2,\ldots} is the number of trials.
#'
#' @details The MultivariateNormal is constructed with a size and probs parameter. Size, number of trials,
#' should not be confused with the \code{K} parameter for number of categories. \code{K} is determined
#' automatically by the number of probabilities supplied to the \code{probs} argument, this also tells the
#' object how many inputs to expect in \code{pdf} and \code{rand}. \code{cdf} and \code{quantile} are omitted
#' as no closed form analytic expression could be found.
#'
#' @name MultivariateNormal
#'
#' @section Constructor: MultivariateNormal$new(size, probs, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab integer \tab number of trials. See details. \cr
#' \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The MultivariateNormal distribution is parameterised by size and prob.
#' Size, N, is given as a single integer greater than zero, such that if \eqn{x} is a vector of \eqn{K} parameters
#' passed to the pmf then it should be true that \eqn{\sum x_i = N}.
#' The length of the probability vector, \eqn{K}, tells the constructor how many arguments to expect
#' to be passed to the pmf function. The probability vector is automatically normalised with
#' \deqn{probs = probs/sum(probs)}.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @export
NULL
#-------------------------------------------------------------
# MultivariateNormal Distribution Definition
#-------------------------------------------------------------
MultivariateNormal <- R6::R6Class("MultivariateNormal", inherit = SDistribution, lock_objects = F)
MultivariateNormal$set("public","name","MultivariateNormal")
MultivariateNormal$set("public","short_name","MultiNorm")
MultivariateNormal$set("public","traits",list(type = Reals$new(dim = "K"),
                                  valueSupport = "continuous",
                                  variateForm = "multivariate"))
MultivariateNormal$set("public","description","Multivariate Normal Probability Distribution.")
MultivariateNormal$set("public","package","distr6")

MultivariateNormal$set("public","mean",function(){
  return(self$getParameterValue("means"))
})
MultivariateNormal$set("public","mode",function(){
  return(self$getParameterValue("means"))
})
MultivariateNormal$set("public","var",function(){
  return(diag(self$cov()))
})
MultivariateNormal$set("public","cov",function(){
  return(matrix(self$getParameterValue("cov"),nrow = self$getParameterValue("K")))
})
MultivariateNormal$set("public","cor",function(){
  return(self$cov() / (sqrt(self$var() %*% t(self$var()))))
})
MultivariateNormal$set("public","entropy",function(base = 2){
  return(0.5 * log(det(2 * pi * exp(1) * self$getParameterValue("cov")), base))
})
MultivariateNormal$set("public", "mgf", function(t){
  return(exp((t(self$getParameterValue("means")) %*% t) + (0.5 * t(t) * self$getParameterValue("cov") * t)))
})
MultivariateNormal$set("public", "cf", function(t){
  return(exp((1i * t(self$getParameterValue("means")) %*% t) + (0.5 * t(t) * self$getParameterValue("cov") * t)))
})

MultivariateNormal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$means)) lst = c(lst, list(means = paramlst$means))
  if(!is.null(paramlst$cov)) lst = c(lst, list(cov = paramlst$cov))
  return(lst)
})

MultivariateNormal$set("public","initialize",function(means = rep(0,2), cov = matrix(c(1,1,1,1),nrow=2),
                                                      decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, means, cov, verbose)
  self$setParameterValue(list(means = means, cov = cov))

  pdf <- function(x1){
    if(isSymmetric.matrix(self$cov()) & all(eigen(self$cov(),only.values = T)$values > 0)){

      if(length(x1) != self$getParameterValue("K"))
        stop(paste("x1 should be of length",self$getParameterValue("K")))

      return(as.numeric((2*pi)^(-self$getParameterValue("K")/2) * det(self$cov())^-0.5 *
               exp((-0.5 * t(x1 - self$getParameterValue("means"))) %*% solve(self$cov()) %*%
                     (x1 - self$getParameterValue("means")))))
    } else
      return(NaN)

  }
  rand <- function(n){
    ch <- chol(self$cov())
    xs <- matrix(rnorm(self$getParameterValue("K")*n), ncol = n)
    return(data.table::data.table(t(means + ch %*% xs)))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Reals$new(dim = length(means)),
                   distrDomain = Reals$new(dim = length(means)), symmetric = FALSE)
  invisible(self)
})
