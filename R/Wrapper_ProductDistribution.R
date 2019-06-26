#' @name ProductDistribution
#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of multiple independent probability distributions.
#' @seealso \code{\link{ArrayDistribution}} and \code{\link{Distribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_P(X1 = x1,...,XN = xN) = f_X1(x1) * ... * f_XN(xn)}
#' \deqn{F_P(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)}
#' where f_P/F_P is the pdf/cdf of the joint (product) distribution P and X1,...,XN are independent distributions.
#'
#' \code{ProductDistribution} inherits all methods from \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
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
#' prodBin <- ProductDistribution$new(list(Binomial$new(prob = 0.5, size = 10),
#'                                    Binomial$new(prob = 0.5, size = 20)))
#' prodBin$pdf(x1 = 2, x2 =3)
#' prodBin$cdf(x1 = 5, x2 = 10)
#' prodBin$rand(10)
NULL

#' @export
ProductDistribution <- R6::R6Class("ProductDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
ProductDistribution$set("public","initialize",function(distlist, name = NULL,
                                                       short_name = NULL, description = NULL){

  distlist = lapply(distlist, function(x) x$clone())
  distlist = makeUniqueDistributions(distlist)

  if(is.null(name)) name = paste("Product:",paste0(lapply(distlist, function(x) x$name),collapse=", "))
  if(is.null(short_name)) short_name = paste0(lapply(distlist, function(x) x$short_name),collapse="X")
  if(is.null(description)) description = paste0("Product of:",paste0(lapply(distlist, function(x) x$description), collapse=" "))

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
    return(apply(y,1,prod))
  },list(n = length(distlist)))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    cdfs = NULL
    for(i in 1:n)
      cdfs = c(cdfs,self$wrappedModels()[[i]]$cdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(cdfs, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(apply(y,1,prod))
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
