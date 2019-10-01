#' @name MixtureDistribution
#' @title Mixture Distribution Wrapper
#' @description Wrapper used to construct a mixture of two or more distributions.
#'
#' @section Constructor: MixtureDistribution$new(distlist, weights = NULL, vectordist = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{weights} \tab numeric \tab Vector of weights. See Details. \cr
#' \code{vectordist} \tab numeric \tab Vector Distribution. See Details. \cr
#' }
#'
#' @details A Mixture Distribution is a weighted combination of two or more distributions such that for
#' pdf/cdfs of n distribution \eqn{f_1,...,f_n}/\eqn{F_1,...,F_n} and a given weight associated to each distribution,
#' \eqn{w_1,...,w_n}. The pdf of the mixture distribution \eqn{M(X1,...,XN)}, \eqn{f_M} is given by
#' \deqn{f_M = \sum_i (f_i)(w_i)}
#' and the cdf, F_M is given by
#' \deqn{F_M = \sum_i (F_i)(w_i)}
#'
#' If weights are given, they should be provided as a vector of numerics. If they don't sum to one
#' then they are normalised automatically. If NULL, they are taken to be uniform, i.e. for n
#' distributions, \eqn{w_i = 1/n, \ \forall \ i \ \in \ [1,n]}{w_i = 1/n, for all i \epsilon [1,n]}.
#'
#' Can optionally be constructed using a \code{VectorDistribution}, in which case \code{distlist} is ignored
#' and the mixture is constructed with the wrapped models in the vector.
#'
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @return Returns an R6 object of class MixtureDistribution.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @examples
#' mixture <- MixtureDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Binomial$new()),
#'                                    weights = c(0.2,0.8))
#' mixture$pdf(1)
#' mixture$cdf(1)
#'
#' @export
NULL
MixtureDistribution <- R6::R6Class("MixtureDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
.distr6$wrappers <- append(.distr6$wrappers, list(MixtureDistribution = MixtureDistribution))

MixtureDistribution$set("public","initialize",function(distlist, weights = NULL, vectordist = NULL){

  name = short_name = description = NULL

  if(!is.null(vectordist)){
    distlist <- vectordist$wrappedModels()
    name = gsub("Vector","Mixture",vectordist$name)
    short_name = gsub("Vec","Mix",vectordist$short_name)
    description = gsub("Vector","Mixture",vectordist$description)
  } else
    distlist <- makeUniqueDistributions(distlist)

  distnames = names(distlist)

  if(is.null(weights))
    weights = "uniform"
  else{
    checkmate::assert(length(weights)==length(distlist))
    weights <- weights/sum(weights)
  }

  private$.weights <- weights

  pdf <- function(x1,...) {
    if(length(x1)==1){
      if(!is.numeric(private$.weights))
        return(as.numeric(mean(sapply(self$wrappedModels(), function(y) y$pdf(x1)))))
      else
        return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$pdf(x1)) * private$.weights)))
    } else{
      if(!is.numeric(private$.weights))
        return(as.numeric(rowMeans(sapply(self$wrappedModels(), function(y) y$pdf(x1)))))
      else
        return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$pdf(x1)) %*% diag(private$.weights))))
    }
  }
  formals(pdf)$self <- self

  cdf <- function(x1,...) {
    if(length(x1)==1){
      if(!is.numeric(private$.weights))
        return(as.numeric(mean(sapply(self$wrappedModels(), function(y) y$cdf(x1)))))
      else
        return(as.numeric(sum(sapply(self$wrappedModels(), function(y) y$cdf(x1)) * private$.weights)))
    } else{
      if(!is.numeric(private$.weights))
        return(as.numeric(rowMeans(sapply(self$wrappedModels(), function(y) y$cdf(x1)))))
      else
        return(as.numeric(rowSums(sapply(self$wrappedModels(), function(y) y$cdf(x1)) %*% diag(private$.weights))))
    }
  }
  formals(cdf)$self <- self

  rand <- function(n){
    x = Multinomial$new(probs = private$.weights, size = n)$rand(1)
    y = c()
    for(i in 1:length(x))
      y = c(y, self$wrappedModels()[[i]]$rand(x[[i]]))
    if(length(y) == 1)
      return(y)
    else
      return(sample(y, n))
  }
  formals(rand)$self <- self

  if(is.null(name)) name = paste("Mixture of",paste(distnames, collapse = " and "))
  if(is.null(short_name)) short_name = paste(distnames, collapse = "Mix")
  type = do.call(union.SetInterval, lapply(distlist, type))
  support = do.call(union.SetInterval, lapply(distlist, type))

  if(is.null(description)) description =  paste0("Mixture of: ",paste0(1:length(distlist),") ",lapply(distlist, function(x) x$description),
                                            collapse = " And "), " - With weights: (",
                       paste0(weights, collapse=", "), ")")

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, rand = rand, name = name,
                   short_name = short_name, description = description, type = type,
                   support = support, valueSupport = "mixture")
})
MixtureDistribution$set("private",".weights",numeric(0))
