#' @name VectorDistribution
#' @title Vector Distribution
#' @description A wrapper for creating a vector of distributions.
#' @seealso \code{\link{ArrayDistribution}} and \code{\link{ProductDistribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details A vector of distributions has the following relationship
#' \deqn{f_V(X1 = x1,...,XN = xN) = f_X1(x1), ..., f_XN(xn)}
#' \deqn{F_V(X1 = x1,...,XN = xN) = F_X1(x1), ..., F_XN(xn)}
#' where f_V/F_V is the pdf/cdf of the vector of distributions V and X1,...,XN are distributions.
#'
#' \code{VectorDistribution} inherits all methods from \code{\link{Distribution}} and
#' \code{\link{DistributionWrapper}}.
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' }
#'
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#'
#' @examples
#' vecBin <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10),
#'                                    Binomial$new(prob = 0.5, size = 20)))
#' vecBin$pdf(x1 = 2, x2 =3)
#' vecBin$cdf(x1 = 5, x2 = 10)
#' vecBin$rand(10)
NULL

#' @export
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
    prods = NULL
    for(i in 1:n)
      prods = c(prods,self$wrappedModels()[[i]]$pdf(get(paste0("x",i))))
    return(prod(prods))
  },list(n = length(distlist)))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    prods = NULL
    for(i in 1:n)
      prods = c(prods,self$wrappedModels()[[i]]$cdf(get(paste0("x",i))))
    return(prod(prods))
  },list(n = length(distlist)))

  rand = function(n) {
    return(data.table::data.table(sapply(self$wrappedModels(), function(x) x$rand(n))))
  }


  type = do.call(product, lapply(distlist,function(x) x$type()))
  support = do.call(product, lapply(distlist,function(x) x$support()))
  distrDomain = do.call(product, lapply(distlist,function(x) x$distrDomain()))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, rand = rand, name = name,
                   short_name = short_name, description = description, support = support, type = type,
                   distrDomain = distrDomain)
}) # IN PROGRESS
