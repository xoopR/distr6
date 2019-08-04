#' @name ProductDistribution
#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of multiple independent probability distributions.
#' @seealso \code{\link{listWrappers}} and \code{\link{VectorDistribution}}
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_P(X1 = x1,...,XN = xN) = f_{X1}(x1) * ... * f_{XN}(xn)}{f_P(X1 = x1,...,XN = xN) = f_X1(x1) * ... * f_XN(xn)}
#' \deqn{F_P(X1 = x1,...,XN = xN) = F_{X1}(x1) * ... * F_{XN}(xn)}{F_P(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)}
#' where \eqn{f_P}/\eqn{F_P} is the pdf/cdf of the joint (product) distribution \eqn{P} and \eqn{X1,...,XN} are independent distributions.
#'
#' \code{ProductDistribution} inherits all methods from \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
#'
#' @section Constructor: ProductDistribution$new(distlist = NULL, distribution = NULL, paramList = NULL, name = NULL, short_name = NULL, description = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{distribution} \tab distribution \tab Distribution to wrap. \cr
#' \code{paramList} \tab list \tab List of parameters, see example. \cr
#' \code{name} \tab list \tab Optional new name for distribution. \cr
#' \code{short_name} \tab list \tab Optional new short_name for distribution. \cr
#' \code{description} \tab list \tab Optional new description for distribution. \cr
#' }
#'
#' @section Constructor Details: A product distribution can either be constructed by a list of
#' distributions passed to \code{distlist} or by passing the name of a distribution implemented in distr6
#' to \code{distribution}, as well as a list of parameters to \code{paramList}. The former case provides more flexibility
#' in the ability to use multiple distributions but the latter is useful for quickly combining many
#' distributions of the same type. See examples.
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @return Returns an R6 object of class ProductDistribution.
#'
#' @examples
#' prodBin <- ProductDistribution$new(list(Binomial$new(prob = 0.5, size = 10),
#'                                    Binomial$new(prob = 0.5, size = 20)))
#' prodBin$pdf(x1 = 2, x2 =3)
#' prodBin$cdf(x1 = 5, x2 = 10)
#' prodBin$rand(10)
#'
#' prodBin = ProductDistribution$new(distribution = Binomial,
#'        paramList = list(list(prob = 0.1, size = 2),
#'                    list(prob = 0.6, size = 4),
#'                    list(prob = 0.2, size = 6)))
#' prodBin$pdf(x1=1,x2=2,x3=3)
#' prodBin$cdf(x1=1,x2=2,x3=3)
#' prodBin$rand(10)
#'
#' @export
NULL
ProductDistribution <- R6::R6Class("ProductDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
ProductDistribution$set("public","initialize",function(distlist = NULL, distribution = NULL, paramList = NULL,
                                                       name = NULL, short_name = NULL, description = NULL){

  if(is.null(distlist)){
    if(is.null(distribution) | is.null(paramList))
      stop("Either distlist or distribution and paramList must be provided.")

    distribution = paste0(substitute(distribution))
    if(!(distribution %in% listDistributions(simplify = T)))
      stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

    distribution = get(distribution)
    distlist = makeUniqueDistributions(sapply(paramList, function(x) do.call(distribution$new, x)))
  }

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


  type = do.call(product.SetInterval, lapply(distlist,function(x) x$type()))
  support = do.call(product.SetInterval, lapply(distlist,function(x) x$support()))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, rand = rand, name = name,
                   short_name = short_name, description = description, support = support, type = type)
})
