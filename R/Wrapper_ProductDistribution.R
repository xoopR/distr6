#' @name ProductDistribution
#' @title Product Distribution
#' @description A wrapper for truncating any probability distribution at given limits.
#' @seealso \code{\link{HuberizedDistribution}} and \code{\link{DistributionWrapper}} for wrapper details.
#' See \code{\link{Distribution}} for a list of public methods.
#' @details Truncates a distribution at lower and upper limits, using the formulae
#' \deqn{f_T = f_X(x) / (F_X(upper) - F_X(lower))}
#' \deqn{F_T = (F_X(x) - F_X(lower)) / (F_X(upper) - F_X(lower))}
#' where f_T/F_T is the pdf/cdf of the truncated distribution T = Truncate(X, lower, upper) and f_X, F_X is the
#' pdf/cdf of the original distribution.
#'
#' If lower or upper are missing they are taken to be \code{self$inf()} and \code{self$sup()} respectively.
#' The support of the new distribution is the interval of points between lower and upper.
#'
#' \code{TruncatedDistribution} inherits all methods from \code{Distribution}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution to truncate. \cr
#' \code{lower} \tab Lower limit for truncation. \cr
#' \code{upper} \tab Upper limit for truncation.
#' }
#'
#' @examples
#' truncBin <- TruncatedDistribution$new(Binomial$new(prob = 0.5, size = 10), lower = 2, upper = 4)
#' truncBin$getParameterValue("truncBin_prob")
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
