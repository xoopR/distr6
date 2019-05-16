#' @name ProductDistribution
#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of two indepdendent probability distributions.
#' @seealso \code{\link{ArrayDistribution}} and \code{\link{Distribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_P(x1, x2) = f_X1(x1) * f_X2(x2)}
#' \deqn{F_P(x1, x2) = F_X1(x1) * F_X2(x2)}
#' where f_P/F_P is the pdf/cdf of the joint (product) distribution P and X1,X2 are independent distributions.
#'
#' \code{ProductDistribution} inherits all methods from \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{dist1} \tab Distribution. \cr
#' \code{dist2} \tab Distribution. \cr
#' }
#'
#' @examples
#' prodBin <- ProductDistribution$new(Binomial$new(prob = 0.5, size = 10),
#' Binomial$new(prob = 0.5, size = 20))
#' prodBin$pdf(x1 = 2, x2 =3)
#' prodBin$cdf(x1 = 5, x2 = 10)
NULL

#' @export
ProductDistribution <- R6::R6Class("ProductDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
ProductDistribution$set("public","initialize",function(dist1, dist2){

  distlist = list(dist1$clone(), dist2$clone())
  distlist = makeUniqueDistributions(distlist)

  name = paste("Product of",distlist[[1]]$short_name,"and",distlist[[2]]$short_name)
  short_name = paste0(distlist[[1]]$short_name,"X",distlist[[2]]$short_name)

  pdf = function(x1,x2) {}
  body(pdf) = substitute({
    return(self$wrappedModels(dist1)$pdf(x1) * self$wrappedModels(dist2)$pdf(x2))
  },list(dist1 = distlist[[1]]$short_name, dist2 = distlist[[2]]$short_name))

  cdf = function(x1,x2) {}
  body(cdf) = substitute({
    return(self$wrappedModels(dist1)$cdf(x1) * self$wrappedModels(dist2)$cdf(x2))
  },list(dist1 = distlist[[1]]$short_name, dist2 = distlist[[2]]$short_name))

  type = distlist[[1]]$type() * distlist[[2]]$type()
  support = distlist[[1]]$support() * distlist[[2]]$support()
  distrDomain = distlist[[1]]$distrDomain() * distlist[[2]]$distrDomain()

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, support = support, type = type,
                   distrDomain = distrDomain)
}) # IN PROGRESS
