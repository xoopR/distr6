#' @name MixtureDistribution
#' @title Mixture Distribution Wrapper
#' @description Wrapper used to construct a mixture of two or more distributions.
#' @format An \code{\link[R6]{R6}} object.
#' @seealso \code{\link{DistributionWrapper}} for wrapper details.
#' See \code{\link{Distribution}} for a list of public methods.
#'
#' @details A Mixture Distribution is a weighted combination of two or more distributions such that for
#' pdf/cdfs of n distribution f_1,...,f_n/F_1,...,F_n and a given weight associated to each distribution,
#' w_1,...,w_n. The pdf of the mixture distribution M(X1,...,XN), f_M is given by
#' \deqn{f_M = sum_i (f_i)(w_i)}
#' and the cdf, F_M is given by
#' \deqn{F_M = sum_i (F_i)(w_i)}
#'
#' If weights are given, they should be provided as a vector of numerics summing to one. If NULL,
#' they are taken to be uniform, i.e. for n distributions, \eqn{w_i = 1/n, \forall i \in [1,n]}.
#'
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{weights} \tab list \tab Vector of weights. See Details. \cr
#' \code{...} \tab any \tab Additional arguments to pass to pdf/cdf.
#' }
#'
#' @section Public Methods:
#' \tabular{ll}{
#' \strong{Method} \tab \strong{Details} \cr
#' \code{weights()} \tab Returns the weights of the wrapped distributions.
#' }
#'
#' @examples
#' mixture <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()))
#' mixture$pdf(1)
#' mixture$cdf(1)
NULL

#' @export
MixtureDistribution <- R6::R6Class("MixtureDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
MixtureDistribution$set("public","initialize",function(distlist, weights = NULL, ...){

  distlist = makeUniqueDistributions(distlist)
  distnames = names(distlist)

  if(is.null(weights))
    weights = rep(1/length(distlist), length(distlist))
  else{
    checkmate::assert(length(weights)==length(distlist))
    checkmate::assert(sum(weights)==1)
  }

  private$.weights <- weights

  pdf <- function(x1,...) {
    if(length(x1)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$pdf(x1)) * self$weights())))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$pdf(x1)) %*% diag(self$weights()))))
  }
  formals(pdf)$self <- self

  cdf <- function(x1,...) {
    if(length(x1)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$cdf(x1)) * self$weights())))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$cdf(x1)) %*% diag(self$weights()))))
  }
  formals(cdf)$self <- self

  name = paste("Mixture of",paste(distnames, collapse = "_"))
  short_name = paste0("Mix_",paste(distnames, collapse = "_"))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, ...)
}) # IN PROGRESS


MixtureDistribution$set("public","weights",function(){
  return(private$.weights)
})
MixtureDistribution$set("private",".weights",numeric(0))
