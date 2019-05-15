#' @name ProductDistribution
#' @title Product Distribution
#' @description A wrapper for creating the joint distribution of two indepdendent probability distributions.
#' @seealso \code{\link{ArrayDistribution}} and \code{\link{Distribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_P(x, y) = f_X(x) * f_Y(y)}
#' \deqn{F_P(x, y) = F_X(x) * F_Y(y)}
#' where f_P/F_P is the pdf/cdf of the joint (product) distribution P and X,Y are independent distributions.
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
#' prodBin$pdf(2, y =3)
#' prodBin$cdf(5, y = 10)
NULL

#' @export
ProductDistribution <- R6::R6Class("ProductDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
ProductDistribution$set("public","initialize",function(dist1, dist2){

  distlist = list(dist1$clone(), dist2$clone())
  distlist = makeUniqueDistributions(distlist)

  name = paste("Product of",distlist[[1]]$short_name,"and",distlist[[2]]$short_name)
  short_name = paste0(distlist[[1]]$short_name,"X",distlist[[2]]$short_name)

  pdf = function(x,y) {}
  body(pdf) = substitute({
    return(self$wrappedModels(dist1)$pdf(x) * self$wrappedModels(dist2)$pdf(y))
  },list(dist1 = distlist[[1]]$short_name, dist2 = distlist[[2]]$short_name))

  cdf = function(x,y) {}
  body(cdf) = substitute({
    return(self$wrappedModels(dist1)$cdf(x) * self$wrappedModels(dist2)$cdf(y))
  },list(dist1 = distlist[[1]]$short_name, dist2 = distlist[[2]]$short_name))

  type = distlist[[1]]$type() * distlist[[2]]$type()
  support = distlist[[1]]$support() * distlist[[2]]$support()
  distrDomain = distlist[[1]]$distrDomain() * distlist[[2]]$distrDomain()

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                   short_name = short_name, support = support, type = type,
                   distrDomain = distrDomain)
}) # IN PROGRESS
