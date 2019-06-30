#' @name MixtureDistribution
#' @title Mixture Distribution Wrapper
#' @description Wrapper used to construct a mixture of two or more distributions.
#'
#' @section Constructor: MixtureDistribution$new(distlist, weights = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{weights} \tab numeric \tab Vector of weights. See Details. \cr
#' }
#'
#' @details A Mixture Distribution is a weighted combination of two or more distributions such that for
#' pdf/cdfs of n distribution f_1,...,f_n/F_1,...,F_n and a given weight associated to each distribution,
#' w_1,...,w_n. The pdf of the mixture distribution M(X1,...,XN), f_M is given by
#' \deqn{f_M = sum_i (f_i)(w_i)}
#' and the cdf, F_M is given by
#' \deqn{F_M = sum_i (F_i)(w_i)}
#'
#' If weights are given, they should be provided as a vector of numerics. If they don't sum to one
#' then they are normalised automatically. If NULL, they are taken to be uniform, i.e. for n
#' distributions, \eqn{w_i = 1/n, \forall i \in [1,n]}.
#'
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}}.
#'
#' @examples
#' mixture <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
#'                                    weights = c(0.2,0.8))
#' mixture$pdf(1)
#' mixture$cdf(1)
NULL

#' @export
MixtureDistribution <- R6::R6Class("MixtureDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
MixtureDistribution$set("public","initialize",function(distlist, weights = NULL){

  distlist = makeUniqueDistributions(distlist)
  distnames = names(distlist)

  if(is.null(weights))
    weights = rep(1/length(distlist), length(distlist))
  else{
    checkmate::assert(length(weights)==length(distlist))
  }

  weights <- weights/sum(weights)
  private$.weights <- weights

  pdf <- function(x1,...) {
    if(length(x1)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$pdf(x1)) * private$.weights)))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$pdf(x1)) %*% diag(private$.weights))))
  }
  formals(pdf)$self <- self

  cdf <- function(x1,...) {
    if(length(x1)==1)
      return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$cdf(x1)) * private$.weights)))
    else
      return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$cdf(x1)) %*% diag(private$.weights))))
  }
  formals(cdf)$self <- self

  rand <- function(n){
    x = Multinomial$new(probs = private$.weights, size = n)$rand(1)
    y = c()
    for(i in 1:length(x))
      y = c(y, self$wrappedModels()[[i]]$rand(x[[i]]))
    return(sample(y, n))
  }
  formals(rand)$self <- self

  name = paste("Mixture of",paste(distnames, collapse = "_"))
  short_name = paste0("Mix_",paste(distnames, collapse = "_"))
  type = do.call(union, lapply(distlist, type))
  support = do.call(union, lapply(distlist, type))
  distrDomain = do.call(union, lapply(distlist, type))

  description =  paste0("Mixture of: ",paste0(1:length(distlist),") ",lapply(distlist, function(x) x$description),
                                            collapse = " And "), " - With weights: (",
                       paste0(weights, collapse=", "), ")")

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, rand = rand, name = name,
                   short_name = short_name, description = description, type = type,
                   support = support, distrDomain = distrDomain)
}) # IN PROGRESS
MixtureDistribution$set("private",".weights",numeric(0))
