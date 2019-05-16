#' @name ArrayDistribution
#' @title Product ArrayDistribution
#' @description A special case joint distribution where each independent distribution is the same
#' Distribution class but not necessarily with the same parameters.
#' @seealso \code{\link{ProductDistribution}} and \code{\link{Distribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_A(X1 = x1,...,XN = xN) = f_X1(x1) * ... * f_XN(xn)}
#' \deqn{F_A(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)}
#' where f_A/F_A is the pdf/cdf of the joint (product) distribution P and X1,...,XN are independent distributions.
#'
#' \code{ArrayDistribution} inherits all methods from \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution. \cr
#' \code{paramList} \tab list of parameters, see examples. \cr
#' }
#'
#'
#' @examples
#' a = ArrayDistribution$new(Binomial, list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
#'                                         list(prob = 0.2, size = 6)))
#' a$pdf(1,2,3)
#' a$cdf(1,2,3)
NULL

#' @export
ArrayDistribution <- R6::R6Class("ArrayDistribution", inherit = DistributionWrapper)
ArrayDistribution$set("public","initialize",function(distribution, paramList){
  distribution = paste0(substitute(distribution))
  if(!(distribution %in% listDistributions(simplify = T)))
    stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

  distribution = get(distribution)

  distlist = makeUniqueDistributions(sapply(paramList, function(x) do.call(distribution$new, x)))

  name = paste("Array of",length(distlist),distribution$public_fields$name,"distributions")
  short_name = paste0("Array",length(distlist),distribution$public_fields$short_name)

  lst <- rep(list(bquote()), length(distlist))
  names(lst) <- paste("x",1:length(distlist),sep="")

  pdf = function() {}
  formals(pdf) = lst
  body(pdf) = substitute({
    prods = NULL
    for(i in 1:n){
      prods = c(prods,self$wrappedModels(paste0(shortname,i))$pdf(get(paste0("x",i))))
    }
    return(prod(prods))
  },list(n = length(distlist), shortname = distribution$public_fields$short_name))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    prods = NULL
    for(i in 1:n){
      prods = c(prods,self$wrappedModels(paste0(shortname,i))$cdf(get(paste0("x",i))))
    }
    return(prod(prods))
  },list(n = length(distlist), shortname = distribution$public_fields$short_name))

  type = do.call(product, lapply(distlist,function(x) x$type()))
  support = do.call(product, lapply(distlist,function(x) x$support()))
  distrDomain = do.call(product, lapply(distlist,function(x) x$distrDomain()))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, support = support, type = type,
                   distrDomain = distrDomain)

})
