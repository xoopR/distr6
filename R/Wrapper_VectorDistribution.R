#' @name VectorDistribution
#' @title Vectorise Distributions
#' @description A wrapper for creating a vector of distributions.
#'
#' @details A vector of distributions has the following relationship
#' \deqn{f_V(X1 = x1,...,XN = xN) = f_X1(x1), ..., f_XN(xn)}
#' \deqn{F_V(X1 = x1,...,XN = xN) = F_X1(x1), ..., F_XN(xn)}
#' where f_V/F_V is the pdf/cdf of the vector of distributions V and X1,...,XN are distributions.
#'
#' @section Constructor: VectorDistribution$new(distlist, name = NULL, short_name = NULL, description = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{name} \tab list \tab Optional new name for distribution. \cr
#' \code{short_name} \tab list \tab Optional new short_name for distribution. \cr
#' \code{description} \tab list \tab Optional new description for distribution. \cr
#' }
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @examples
#' vecBin <- VectorDistribution$new(list(Binomial$new(prob = 0.5,
#'                            size = 10), Normal$new(mean = 15)))
#' vecBin$pdf(x1 = 2, x2 =3)
#' vecBin$cdf(1:5, 12:16)
#' vecBin$quantile(c(0.1,0.2),c(0.3,0.4))
#' vecBin$rand(10)
#'
#' @export
NULL
VectorDistribution <- R6::R6Class("VectorDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
VectorDistribution$set("public","initialize",function(distlist, name = NULL,
                                                       short_name = NULL, description = NULL){

  distlist = lapply(distlist, function(x) x$clone())
  distlist = makeUniqueDistributions(distlist)

  if(is.null(name)) name = paste("Vector:",paste0(lapply(distlist, function(x) x$name),collapse=", "))
  if(is.null(short_name)) short_name = paste0(lapply(distlist, function(x) x$short_name),collapse="X")
  if(is.null(description)) description = paste0("Vector of:",paste0(lapply(distlist, function(x) x$description), collapse=" "))

  lst <- rep(list(bquote()), length(distlist))
  names(lst) <- paste("x",1:length(distlist),sep="")

  pdf = function() {}
  formals(pdf) = lst
  body(pdf) = substitute({
    pdfs = NULL
    for(i in 1:n)
      pdfs = c(pdfs,self$wrappedModels()[[i]]$pdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(pdfs, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    cdfs = NULL
    for(i in 1:n)
      cdfs = c(cdfs,self$wrappedModels()[[i]]$cdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(cdfs, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  quantile = function() {}
  formals(quantile) = lst
  body(quantile) = substitute({
    quantiles = NULL
    for(i in 1:n)
      quantiles = c(quantiles,self$wrappedModels()[[i]]$quantile(get(paste0("x",i))))
    y = data.table::data.table(matrix(quantiles, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  rand = function(n) {
    return(data.table::data.table(sapply(self$wrappedModels(), function(x) x$rand(n))))
  }


  type = do.call(product.SetInterval, lapply(distlist,function(x) x$type()))
  support = do.call(product.SetInterval, lapply(distlist,function(x) x$support()))
  distrDomain = do.call(product.SetInterval, lapply(distlist,function(x) x$distrDomain()))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand, name = name,
                   short_name = short_name, description = description, support = support, type = type,
                   distrDomain = distrDomain)
})
